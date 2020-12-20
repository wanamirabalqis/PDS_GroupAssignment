# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
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
library(flexdashboard)
require(shinydashboard)
#recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)

#head(recommendation)
url <- paste( "C:/Users/KANGHEA/OneDrive - Hilti/Study/WQD7001",
              "/Assignment/Group Assignment", 
              "/superStoreData.csv", sep = "")

superStoreData <- read.csv(url)

stateSales <- superStoreData %>%                     # Specify data frame
  group_by(State,Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))

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

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Basic Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.salesforce.com")
  )
)


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
    title = "Sales by Product"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("revenuebyPrd", height = "300px")
  )
  
  ,box(
    title = "Sales by State"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,highchartOutput("revenuebyRegion", height = "300px")
  ) 
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  #total.revenue <- sum(recommendation$Revenue)
  #sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  #prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderPlot({
    
    regionCatProfit <- superStoreData %>%                     # Specify data frame
      group_by(Region,Category) %>%                   # Specify group indicator
      summarise_at(vars(Profit),              # Specify column
                   list(totalProfit = sum))
    
    ggplot(regionCatProfit, aes(x = Region, y = totalProfit))+
      geom_col(aes(fill = Category), width = 0.7)
  })
  
  
  output$revenuebyRegion <- renderHighchart({
    
    mapSales <- stateSales %>%
      group_by(State) %>%                   # Specify group indicator
      summarise_at(vars(totalSales),              # Specify column
                   list(totalSales = sum))
    
    mapSales
    
    highchart() %>%
      hc_add_series_map(usgeojson, mapSales, name = "Total Sales",
                        value = "totalSales", joinBy = c("woename", "State"),
                        dataLabels = list(enabled = TRUE,
                                          format = '{point.properties.postalcode}')) %>%
      hc_colorAxis(stops = colstops) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_add_theme(thm)  
  })
  
  
  output$trendTotalSales <- renderPlot({
    
    totalSalesYear <- superStoreData %>%                     # Specify data frame
      group_by(month, year) %>%                   # Specify group indicator
      summarise_at(vars(Sales),              # Specify column
                   list(totalSales = sum)) 
    totalSalesYear$month = as.factor(totalSalesYear$month)
    totalSalesYear$year = as.factor(totalSalesYear$year)
    # Visualization
    #totalSalesYear %>%
      #ggplot( aes(x=month, y=totalSales, group=year, color=year)) +
      #geom_line()

    ggplot(totalSalesYear, aes(x = month, y = totalSales)) + 
      geom_line(aes(color = year,group=year)) 
  })
  
}


shinyApp(ui, server)