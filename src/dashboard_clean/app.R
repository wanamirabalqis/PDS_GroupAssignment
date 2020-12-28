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
url <- paste( "/Users/balqishakim/Documents/UM/Semester 1 - MDS/Principles of Data Science/Group Project/US Superstore data.csv", sep = "")

superStoreData <- read.csv(url)
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
stateSales <- superStoreData %>%                     # Specify data frame
  group_by(State,Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))
#============ State Sales Data ENd==================


#============ Category Sales Data Start==============
regionCatProfit <- superStoreData %>%                     # Specify data frame
  group_by(Region,Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))
#============ Category Sales Data End==============
tab2ProductData <- superStoreData %>% 
  group_by(Region,Category,Sub.Category,Product.Name,month) %>%  
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))
tab2ProductData$month = as.factor(tab2ProductData$month)

tab2ProductData <- tab2ProductData %>%
  mutate_at('Product.Name',str_trunc, width = 20, side='right')

str(tab2ProductData$Product.Name)
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
    title = "Top Sales by CAtegory"
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
#============= tab 2 code start ===================
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
    sidebarPanel(),
    mainPanel()
  )
)

History <- fluidPage(
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
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
      if (input$cat == "All"){
        tolSalYearGrp <- totalSalesYear %>%                     
          group_by(month, year) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        return(tolSalYearGrp)
      }
      tolSalYearGrp <- totalSalesYear %>% 
        filter(Category == input$cat) %>%
        group_by(month, year) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      return(tolSalYearGrp)
      
    }
  )
  
  regCatSalFiltered <- reactive(
    {
      if (input$cat == "All"){
        return(regionCatProfit)
      }
      regCatSalGrp <- regionCatProfit %>%
        filter(Category == input$cat)
      
      return(regCatSalGrp)
    }
  )
  
  mapfiltered <- reactive({
    
    #Second aggragation
    if (input$cat == "All"){
      
      mapSales <- stateSales %>%
        group_by(State) %>%                   # Specify group indicator
        summarise_at(vars(totalSales),              # Specify column
                     list(totalSales = sum))
      
      return(mapSales)
    }
    mapSales <- stateSales %>%
      filter(Category == input$cat) %>%
      group_by(State) %>%                   # Specify group indicator
      summarise_at(vars(totalSales),              # Specify column
                   list(totalSales = sum))
  })
  #=========== Tab 1 related code end ===================#
  
  #=========== Tab 2 related code start ===================#
  catSalesTrendFil <- reactive(
    {
      #Second aggragation
      if (input$cat2 == "All"){
        tolCatSalGrp <- tab2ProductData %>%                     
          group_by(Category, month) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        return(tolCatSalGrp)
      }
      tolCatSalGrp <- tab2ProductData %>% 
        filter(Category == input$cat2) %>%
        group_by(Category,month) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      return(tolCatSalGrp)
      
    }
  )
  
  topProdFil <- reactive(
    {
      #Second aggragation
      if (input$cat2 == "All"){
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
      topProdGrp <- tab2ProductData %>% 
        filter(Category == input$cat2) %>%
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
      if (input$cat2 == "All"){
        topSubCatGrp <- tab2ProductData %>%                     
          group_by(Sub.Category,Category) %>%                   
          summarise_at(vars(totalSales),              
                       list(totalSales = sum))
        arrange(topSubCatGrp,totalSales)
        topSubCatGrp = tail(topSubCatGrp)
        
        return(topSubCatGrp)
      }
      topSubCatGrp <- tab2ProductData %>% 
        filter(Category == input$cat2) %>%
        group_by(Sub.Category,Category) %>%                   
        summarise_at(vars(totalSales),              
                     list(totalSales = sum))
      arrange(topSubCatGrp,totalSales)
      topSubCatGrp = tail(topSubCatGrp)
      return(topSubCatGrp)
      
    }
  )
  #=========== Tab 2 related code end ===================#
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
  
  #================= Output UI End========================
}

shinyApp(ui, server)