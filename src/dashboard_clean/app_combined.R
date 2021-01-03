library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)
library(highcharter)
library(dplyr)
library(viridisLite)
library(forecast)
library(treemap)
library(shinydashboard)
library(stringr)

#============= For USA map usage start==========
n <- 4
colstops <- data.frame(
  q = 0:n/n,
  c = substring(viridis(n + 1), 0, 7)) %>%
  list_parse2()

thm <- 
  hc_theme(
    colors = c("#1a6ecc", "#434348", "#90ed7d"),
    chart = list(
      backgroundColor = "transparent",
      style = list(fontFamily = "Source Sans Pro")
    ),
    xAxis = list(
      gridLineWidth = 1
    )
  )
#============= For USA map usage end==========


#============= Prepare data start===================
#============= Prepare data start===================

## Superstore Data
url <- paste( "C:/Users/KANGHEA/OneDrive - Hilti/Study/WQD7001",
              "/Assignment/Group Assignment", 
              "/superStoreData.csv", sep = "")
#url <- paste("C:/Users/MLee27/Desktop/Training/06-Jupyter_Practice/10_UMS/WQD7001_Principal_Data_Science/Project_Data/data_local/Generated",
#             "/US Superstore data_v2.csv", sep = "")

superStoreData <- read.csv(url)

#str(superStoreData)

## US_Census_Merge_Clean_Data
#url <- paste("C:/Users/MLee27/Desktop/Training/06-Jupyter_Practice/10_UMS/WQD7001_Principal_Data_Science/Project_Data/data_local/Generated",
#             "/US_Census_Annual_Merge_Clean_v2.csv", sep = "")

#US_Census_Merge_clean <- read.csv(url)

#str(US_Census_Merge_clean)

## Sales Trend Industry Overlook
#url <- paste("C:/Users/MLee27/Desktop/Training/06-Jupyter_Practice/10_UMS/WQD7001_Principal_Data_Science/Project_Data/data_local/Generated",
#             "/salesbcll.csv", sep = "")

#salesbcll <- read.csv(url)

#str(salesbcll)

#============ Total Sales Year Trend Data Start ==================
totalSalesYear <- superStoreData %>%
  group_by(Category, month, year) %>%                   
  summarise_at(vars(Sales),              
               list(totalSales = sum))
totalSalesYear$month = as.factor(totalSalesYear$month)
totalSalesYear$year = as.factor(totalSalesYear$year)
#============ Total Sales Year Trend Data End ==================

#============ State Sales Data Start ==================
#Prepare data first so that don't need to keep executing the 
#aggregation
stateSales <- superStoreData %>%                     
  group_by(State,Category,year) %>%                   
  summarise_at(vars(Sales),              
               list(totalSales = sum))
#============ State Sales Data ENd==================


#============ Category Sales Data Start==============
regionCatProfit <- superStoreData %>%                     
  group_by(Region,Category,year) %>%                   
  summarise_at(vars(Sales),              
               list(totalSales = sum))
#============ Category Sales Data End==============
tab2ProductData <- superStoreData %>% 
  group_by(Region,Category,Sub.Category,Product.Name,month,year) %>%  
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))
tab2ProductData$month = as.factor(tab2ProductData$month)

tab2ProductData <- tab2ProductData %>%
  mutate_at('Product.Name',str_trunc, width = 20, side='right')
#============ tab2 data start ====================



#============ tab2 data end ======================

#============= Prepare data end===================

#============= UI code end =====================
#============= tab 1 code start ===================
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow2 <- fluidRow(
  box(
    width=12,
    title = "Sales Trend by Year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("trendTotalSales")
  )
  
)

frow3 <- fluidRow(
  
  box(
    title = "Total Sales by Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("totalSalesByPrd", height = "300px")
  )
  
  ,box(
    title = "Sales by State"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,highchartOutput("stateMap", height = "300px")
  ) 
  
)
#============= tab 1 code end ===================

#============= tab 2 code start ===================
frow4 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow5 <- fluidRow(
  box(
    width=12,
    title = "Category Sales Trend"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("catSalesTrend")
  )
  
)

frow6 <- fluidRow(
  
  box(
    title = "Top Sales by Category"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("topSalesCat", height = "300px")
  )
  
  ,box(
    title = "Top Sales by Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("topSalesProd", height = "300px")
  ) 
  
)
#============= tab 2 code end ===================

#============= tab 3 code start ===================

frow7 <- fluidRow(
  box(
    width=12,
    title = "Sales Trend per Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("histogram",height = "300px")
  )
  
)

frow8 <- fluidRow(
  
  box(
    title = "Annual OPEX & Gross Margin from 2008 to 2014"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AnnOPEXGm", height = "200px")
  )
  
  ,box(
    title = "Annual Account Receivable Turnover Ratio (%) from 2008 to 2014"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AnnArRatio", height = "200px")
  )
  
  ,box(
    title = "Annual Gross Margin Percentage (%) from 2008 to 2014"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AnnGmPer", height = "200px")
  )
  
  ,box(
    title = "End of Year Inventories from 2008 to 2014"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("AnnInv", height = "200px")
  )
) 


#============= tab 3 code end ===================
#============= tab 4 code start ===================
frow9 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow10 <- fluidRow(
  box(
    width=12,
    title = "Delivery Trend by Month"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("deliveryMonth")
  )
  
)

frow11 <- fluidRow(
  
  box(
    title = "Shipment Days by Region"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("deliveryRegion", height = "300px")
  )
  
  ,box(
    title = "Shipment Days By Shipping Mode"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("deliverySubCat", height = "300px")
  ) 
  
)
#============= tab 4 code end ===================
#============= UI code end =====================

#=============== Overview UI start===============
overview <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Filter"),
      selectInput("cat", "Category",c("All",
                                      "Furniture",
                                      "Office Supplies",
                                      "Technology")),
      selectInput("year1", "Year",c("All",
                                    "2014",
                                    "2015",
                                    "2016",
                                    "2017")),
      ),
    mainPanel( 
      frow1, 
      frow2, 
      frow3,
    )
  )
)

Product <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Filter"),
      selectInput("cat2", "Category",c("All",
                                       "Furniture",
                                       "Office Supplies",
                                       "Technology")),
      selectInput("year2", "Year",c("All",
                                    "2014",
                                    "2015",
                                    "2016",
                                    "2017")),
    ),
    mainPanel( 
      #frow4, 
      frow5, 
      frow6,
    )
  )
)

Delivery <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Filter"),
      selectInput("cat4", "category",c("All",
                                       "Furniture",
                                       "Office Supplies",
                                       "Technology")),
      selectInput("year4", "Year",c("All",
                                    "2014",
                                    "2015",
                                    "2016",
                                    "2017")),
    ),
    mainPanel( 
      #frow9, 
      frow10, 
      frow11,
    )
  )
)

History <- fluidPage(
  titlePanel("Industry Overlook from 2008 to 2018"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId="yearInput", label="Year Range", value=c(2010, 2014), min=2008, max=2018, step=1),
      selectInput("monthInput", "Month",
                  choices = c("Jan", "Feb", "Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")),
    ),
    mainPanel( 
      frow7
      )
  ),
  
  
  sidebarLayout(
    sidebarPanel(
      titlePanel("Filter"),
      selectInput("cat3", "Category",c("All",
                                       "Furniture",
                                       "Office Supplies",
                                       "Technology")),
    ),
    mainPanel( 
      frow8
    )
  )
)



#================ Overview UI end=================

report <- fluidPage(
  tabBox(
    # The id lets us use input$tabset1 on the server to find the current tab
    id = "tabset1", width=12,
    tabPanel("Overview", overview),
    tabPanel("Product Dashboard", Product),
    tabPanel("Delivery Dashboard", Delivery),
    tabPanel("Industry Overlook", History),
    tabPanel(a("Github", href="https://github.com/wanamirabalqis/PDS_GroupAssignment", target="_blank"))
)
)
#============= UI code end ===================

#============== Dashboard code start==============
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "US Superstore Dashboard (2014-2017)",
                          titleWidth = 450) 

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(report)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')
#============== Dashboard code end ===============

server <- function(input, output, session) {
  
  #============== Reactive Code Start ===============
  #=========== Tab 1 related code start ===================#
  tolSalFiltered <- reactive(
    {
      #Second aggragation
      if (input$cat == "All" & input$year1 == "All"){
        tolSalYearGrp <- totalSalesYear %>%                     
          group_by(month, year) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        
        return(tolSalYearGrp)
      }
      
      tolSalYearGrp = totalSalesYear
      
      if( input$cat != "All" ){
        
        tolSalYearGrp <- tolSalYearGrp %>% 
          filter(Category == input$cat)
        
      }
      
      if (input$year1 != "All") {
        tolSalYearGrp <- tolSalYearGrp %>% 
          filter(year == input$year1)
      }
      
      tolSalYearGrp <-  tolSalYearGrp %>% 
        group_by(month, year) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      
      return(tolSalYearGrp)
      
    }
  )
  
  regCatSalFiltered <- reactive(
    {
      if (input$cat == "All" & input$year1 == "All"){
        
        regCatSalGrp <- regionCatProfit %>%
          group_by(Region,Category) %>%                   
          summarise_at(vars(totalSales), 
                       list(totalSales = sum))
        
        return(regCatSalGrp)
      }
      
      regCatSalGrp <- regionCatProfit
      
      if( input$cat != "All" ){
        
        regCatSalGrp <- regCatSalGrp %>%
          filter(Category == input$cat)
        
      }
      
      if (input$year1 != "All") {
        regCatSalGrp <- regCatSalGrp %>%
          filter(year == input$year1)
      }
      
      regCatSalGrp <- regCatSalGrp %>%
        group_by(Region,Category) %>%                   
        summarise_at(vars(totalSales), 
                     list(totalSales = sum))
      return(regCatSalGrp)
    }
  )
  
  mapfiltered <- reactive({
    
    #Second aggragation
    if (input$cat == "All"  & input$year1 == "All"){
      
      mapSales <- stateSales %>%
        group_by(State) %>%                   # Specify group indicator
        summarise_at(vars(totalSales),              # Specify column
                     list(totalSales = sum))
      
      return(mapSales)
    }
    
    mapSales <- stateSales
    
    if( input$cat != "All" ){
      mapSales <- mapSales %>%
        filter(Category == input$cat)
    }
    
    if (input$year1 != "All") {
      mapSales <- mapSales %>%
        filter(year == input$year1)
    }
    
    mapSales <- mapSales %>%
      group_by(State) %>%                   
      summarise_at(vars(totalSales), 
                   list(totalSales = sum))
  })
  #=========== Tab 1 related code end ===================#
  
  #=========== Tab 2 related code start ===================#
  catSalesTrendFil <- reactive(
    {
      #Second aggragation
      if (input$cat2 == "All" & input$year2 == "All"){
        tolCatSalGrp <- tab2ProductData %>%                     
          group_by(Category, month) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        return(tolCatSalGrp)
      }
      
      tolCatSalGrp <- tab2ProductData
      
      if( input$cat2 != "All" ){
        tolCatSalGrp <- tolCatSalGrp %>% 
          filter(Category == input$cat2)
        
      }
      
      if (input$year2 != "All") {
        tolCatSalGrp <- tolCatSalGrp %>% 
          filter(year == input$year2)
      }
      
      
      tolCatSalGrp <-  tolCatSalGrp %>% 
        group_by(Category,month) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      
      return(tolCatSalGrp)
      
    }
  )
  
  topProdFil <- reactive(
    {
      #Second aggragation
      if (input$cat2 == "All" & input$year2 == "All"){
        topProdGrp <- tab2ProductData %>%                     
          group_by(Product.Name,Category) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        arrange(topProdGrp,totalSales)
        topProdGrp = tail(topProdGrp)
        
        arrange(topProdGrp,totalSales)
        topProdGrp = tail(topProdGrp)
        
        return(topProdGrp)
      }
      
      topProdGrp <- tab2ProductData
      
      if( input$cat2 != "All" ){
        
        topProdGrp <- topProdGrp %>% 
          filter(Category == input$cat2)
      }
      
      if (input$year2 != "All") {
        topProdGrp <- topProdGrp %>% 
          filter(year == input$year2)
        
      }
      
      
      topProdGrp <- topProdGrp %>% 
        group_by(Product.Name,Category) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      
      arrange(topProdGrp,totalSales)
      topProdGrp = tail(topProdGrp)
      
      return(topProdGrp)
      
    }
  )  
  
  topSubCatFil <- reactive(
    {
      #Second aggragation
      if (input$cat2 == "All" & input$year2 == "All"){
        topSubCatGrp <- tab2ProductData %>%                     
          group_by(Sub.Category,Category) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        arrange(topSubCatGrp,totalSales)
        topSubCatGrp = tail(topSubCatGrp)
        
        return(topSubCatGrp)
      }
      
      topSubCatGrp <- tab2ProductData
      
      if( input$cat2 != "All" ){
        topSubCatGrp <- topSubCatGrp %>% 
          filter(Category == input$cat2)
        
      }
      
      if (input$year2 != "All") {
        topSubCatGrp <- topSubCatGrp %>% 
          filter(year == input$year2)
      }
      
      topSubCatGrp <- topSubCatGrp %>% 
        group_by(Sub.Category,Category) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      
      arrange(topSubCatGrp,totalSales)
      topSubCatGrp = tail(topSubCatGrp)
      return(topSubCatGrp)
      
    }
  )
  #=========== Tab 2 related code end ===================#
  
  #=========== Tab 3 related code start ===================#
  filtered <- reactive({
    print(input$monthInput)
    if (is.null(input$yearInput)) {
      print("All is working")
      return(salesbcll)
    }    
    print(input$yearInput[1])
    subset.salesbcll <- salesbcll %>%
      filter(between(Year, input$yearInput[1],
                     input$yearInput[2]),
             Month == input$monthInput,
      )
    return(subset.salesbcll)
  })
  
  AnnOPEXGmFil <- reactive(
    {   print('Working')
        if (input$cat3 == "All"){
        AnnOPEXGmGrp <- US_Census_Merge_clean %>%                     
          group_by(Category)                   
        return(AnnOPEXGmGrp)
      }
      AnnOPEXGmGrp <- US_Census_Merge_clean %>% 
        filter(Category == input$cat3) %>%
        group_by(Category)                   
      return(AnnOPEXGmGrp)
      
    }
  )
  
  AnnGmPerFil <- reactive(
    {   print('Working')
      if (input$cat3 == "All"){
        AnnGmPerGrp <- US_Census_Merge_clean %>%                     
          group_by(Category)                   
        return(AnnGmPerGrp)
      }
      AnnGmPerGrp <- US_Census_Merge_clean %>% 
        filter(Category == input$cat3) %>%
        group_by(Category)                   
      return(AnnGmPerGrp)
    }
  )
  
  AnnInvFil <- reactive(
    {   print('Working')
      if (input$cat3 == "All"){
        AnnInvGrp <- US_Census_Merge_clean %>%                     
          group_by(Category)                   
        return(AnnInvGrp)
      }
      AnnInvGrp <- US_Census_Merge_clean %>% 
        filter(Category == input$cat3) %>%
        group_by(Category)                   
      return(AnnInvGrp)
    }
  )
  
  AnnARFil <- reactive(
    {   print('Working')
      if (input$cat3 == "All"){
        AnnARGrp <- US_Census_Merge_clean %>%                     
          group_by(Category)                   
        return(AnnARGrp)
      }
      AnnARGrp <- US_Census_Merge_clean %>% 
        filter(Category == input$cat3) %>%
        group_by(Category)                   
      return(AnnARGrp)
    }
  )
  
  #=========== Tab 3 related code end ===================#
  #============ Delivery tab related code start==============#
  regionDelvShipFil <- reactive({
    #Second aggragation
    if (input$cat4 == "All" & input$year4 == "All"){
      regDelvDayGrp <- superStoreData %>% 
        group_by(Region,shipDays) %>%                   
        summarise(uniqueOrderId = n_distinct(Order.ID))
      
      return(regDelvDayGrp)
    }
    
    regDelvDayGrp <- superStoreData
    
    if( input$cat4 != "All" ){
      
      regDelvDayGrp <-  regDelvDayGrp %>% 
        filter(Category == input$cat4)
    }
    
    if (input$year4 != "All") {
      regDelvDayGrp <-  regDelvDayGrp %>% 
        filter(year == input$year4)
    }
    
    regDelvDayGrp <-  regDelvDayGrp %>% 
      group_by(Region,shipDays) %>%                   
      summarise(uniqueOrderId = n_distinct(Order.ID))
    
    return(regDelvDayGrp)
  })
  
  monthDelvShipFil <- reactive({
    if (input$cat4 == "All" & input$year4 == "All"){
      monDelvDayGrp <- superStoreData %>%  
        group_by(month,shipDays) %>%                   
        summarise(uniqueOrderId = n_distinct(Order.ID))
      
      monDelvDayGrp$month = as.factor(monDelvDayGrp$month)
      return(monDelvDayGrp)
    }
    
    monDelvDayGrp <- superStoreData
    
    if( input$cat4 != "All" ){
      monDelvDayGrp <- monDelvDayGrp %>%                     
        filter(Category == input$cat4)
      
    }
    
    if (input$year4 != "All") {
      monDelvDayGrp <- monDelvDayGrp %>%                     
        filter(year == input$year4)
      
    }
    
    monDelvDayGrp <- monDelvDayGrp %>%                     
      group_by(month,shipDays) %>%                   
      summarise(uniqueOrderId = n_distinct(Order.ID))
    monDelvDayGrp$month = as.factor(monDelvDayGrp$month)
    return(monDelvDayGrp)
    
  })
  
  subCatDelvShipFil <- reactive({
    if (input$cat4 == "All" & input$year4 == "All"){
      subCatDelvDayGrp <- superStoreData %>%
        group_by(Ship.Mode,shipDays) %>%
        summarise(uniqueOrderId = n_distinct(Order.ID))
      return(subCatDelvDayGrp)
    }
    
    subCatDelvDayGrp <- superStoreData
    
    if( input$cat4 != "All" ){
      subCatDelvDayGrp <-  subCatDelvDayGrp %>%
        filter(Category == input$cat4)
      
    }
    
    if (input$year4 != "All") {
      subCatDelvDayGrp <-  subCatDelvDayGrp %>%
        filter(year == input$year4)
    }
    
    subCatDelvDayGrp <-  subCatDelvDayGrp %>%
      group_by(Ship.Mode,shipDays) %>%
      summarise(uniqueOrderId = n_distinct(Order.ID))
    return(subCatDelvDayGrp)
    
  })
  
  #============ Delivery tab related code end================#
  
  #============== Reactive Code End =============== 
  
  #================= Output UI Start========================
  #=========== Tab 1 related code start ===================#
  output$trendTotalSales <- renderPlot({
    ggplot(tolSalFiltered(), aes(x = month, y = totalSales)) + 
      geom_line(aes(color = year,group=year)) 
  })
  
  output$totalSalesByPrd <- renderPlot({
    ggplot(regCatSalFiltered(), aes(x = Region, y = totalSales))+
      geom_col(aes(fill = Category), width = 0.7)
  })
  
  output$stateMap <- renderHighchart(
    {
      highchart() %>%
        hc_add_series_map(usgeojson, mapfiltered(), name = "Total Sales",
                          value = "totalSales", joinBy = c("woename", "State"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>%
        hc_colorAxis(stops = colstops) %>%
        hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_add_theme(thm)  
    })
  #=========== Tab 1 related code end ===================#
  #=========== Tab 2 related code start ===================#
  output$catSalesTrend <- renderPlot({
    ggplot(catSalesTrendFil(), aes(x = month, y = totalSales)) + 
      geom_line(aes(color = Category,group=Category)) 
  })
  
  output$topSalesCat <- renderPlot({
    ggplot(topSubCatFil(), aes(x = Sub.Category, y = totalSales)) +
      geom_col(aes(fill = Category), width = 0.7)
  })
  
  output$topSalesProd <- renderPlot({
    ggplot(topProdFil(), aes(x = Product.Name, y = totalSales))+
      geom_col(aes(fill = Category), width = 0.7) + coord_flip()
  })
  
  #=========== Tab 2 related code end  ===================#
  
  #=========== Tab 3 related code start  ===================#
  output$histogram <- renderPlot({
    ggplot(filtered(), aes(x= Product, y= Sales)) + 
      geom_col() + theme(text=element_text(size=20)) 
})
  
  ## Gross Margin Percentage is Net Sales - Cost of Goods Sold
  output$AnnOPEXGm <- renderPlot({
    ggplot(data= AnnOPEXGmFil(),aes(x=Year, y=Annual.OPEX.in.million.dollars,fill= Category))+
      geom_bar(position="dodge", stat = "identity")+
      geom_line(aes(x=Year, y=Annual.Gross.Margin.in.million.dollars, color = Category))+
      ggtitle("Annual OPEX (Bar) & Annual Gross Margin (Line)")+
      ylab("Million Dollars ($)")+ theme(text=element_text(size=15)) 
  })
  
  
  ## Gross Margin Percentage is (Net Sales-Cost of Goods Sold)/Net Sales 
  ## Higher gross margin because of its reduces costs of goods sold
  ## Calculate amount of money left over from product sales after subtract cost of goods sold
  output$AnnGmPer <- renderPlot({
    ggplot(data=AnnGmPerFil(), aes(x= Year, y=Annual.Gross.Margin.as.Percentage,color= Category)) + geom_line()+geom_point()+
      ggtitle("Annual Gross Margin Percentage") +
      ylab("Percentage (%)") + theme(text=element_text(size=15))
  })
  
  ## End of Inventory is the value of stock or product that remains at end of an accounting period
  ## It is also important to a business because ending inventory carries over to the new accounting period.
  ## An inaccurate measure of stock value would then continue to have financial implications into the new accounting period.
  output$AnnInv <- renderPlot({
    ggplot(data=AnnInvFil(), aes(x= Year, y=End.of.Year.Inventories.in.million.dollars,color= Category)) + geom_line()+geom_point()+
      ggtitle("End of Year Inventories") +
      ylab("Million Dollars ($)") + theme(text=element_text(size=15)) 
    
  })
  
  
  ## Receivable Turnover ratio is Sales/Total Account Receivable
  ## Measure how effective company in collecting debt, and use assets to generate revenue
  
  output$AnnArRatio <- renderPlot({
  ggplot(data= AnnARFil(),aes(x=Year, y=ARTurnOver,fill= Category))+
    geom_bar(position="dodge", stat = "identity")+
    geom_hline(yintercept=60, color="black", linetype="dashed",size=.5) +
    ggtitle("Annual Account Receivable Turnover Ratio")+
    theme(text=element_text(size=15)) +
    ylab("TurnOver Ratio (%)")
  })
  
  #=========== Tab 3 related code end  ===================#
  
  #=========== Delivery tab related code start============#
  
  output$deliveryRegion <- renderPlot({
    ggplot(regionDelvShipFil(), aes(x = Region, y = uniqueOrderId, fill = shipDays))+
      #  geom_col(aes(fill = shipDays), width = 0.7)
      geom_bar(stat = "identity", position = 'dodge')
  })
  
  output$deliveryMonth <- renderPlot({
    ggplot(monthDelvShipFil(), aes(x = month, y = uniqueOrderId, fill = shipDays))+
      #  geom_col(aes(fill = shipDays), width = 0.7)
      geom_bar(stat = "identity", position = 'dodge')
  })
  
  output$deliverySubCat <- renderPlot({
    ggplot(subCatDelvShipFil(), aes(x = Ship.Mode, y = uniqueOrderId, fill = shipDays))+
      #geom_bar(stat = "identity", position = 'dodge')
      geom_col(aes(fill = shipDays), width = 0.7)
  })
  
  #=========== Delivery tab realted code end==============#
  
  #================= Output UI End========================
}

shinyApp(ui, server)

