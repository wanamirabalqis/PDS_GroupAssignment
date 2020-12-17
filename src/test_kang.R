#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(leaflet)

url <- paste( "C:/Users/KANGHEA/OneDrive - Hilti/Study/WQD7001",
              "/Assignment/Group Assignment", 
              "/US Superstore data.csv", sep = "")

superStoreData <- read.csv(url)

regionCatProfit <- superStoreData %>%                     # Specify data frame
    group_by(Region,Category) %>%                   # Specify group indicator
    summarise_at(vars(Profit),              # Specify column
                 list(totalProfit = sum))
#Prepare data first so that don't need to keep executing the 
#aggregation


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("US Super Store Analysis"),
    
    # Show the plot
    mainPanel(
        selectInput("cat", "category",c("All",
                                        "Furniture",
                                        "Office Supplies",
                                        "Technology")),
        plotOutput("catplot"),
        tableOutput("results")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    filtered <- reactive({
        if (input$cat == "All"){
            return(regionCatProfit)
        }
        regionCatProfit <- regionCatProfit %>%
            filter(Category == input$cat)
    })
    
    output$catplot <- renderPlot(
        {
            ggplot(filtered(), aes(x = Region, y = totalProfit))+
            geom_col(aes(fill = Category), width = 0.7)
        })
}
# Run the application 

shinyApp(ui = ui, server = server)
