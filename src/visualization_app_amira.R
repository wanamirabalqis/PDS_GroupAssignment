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
library("dplyr")
library("lubridate")
library("leaflet")

url <- paste( "/Users/balqishakim/Documents/UM/Semester 1 - MDS/Principles of Data Science/Group Project/US Superstore data.csv", sep = "")

superStoreData <- read.csv(url)

str(superStoreData)

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
           
           leafletOutput("map")
        )
    )


server <- function(input, output) {
    
    output$catplot <- renderPlot(
                {if (input$cat != "All") {
                    categoryfilter <- filter(regionCatProfit, Category == input$cat)}
                
                regionCatProfit <- superStoreData %>%                     # Specify data frame
                group_by(Region,Category) %>%                   # Specify group indicator
                summarise_at(vars(Profit),              # Specify column
                             list(totalProfit = sum))
            
            ggplot(regionCatProfit, aes(x = Region, y = totalProfit))+
                geom_col(aes(fill = Category), width = 0.7)})
    
    output$map <- renderLeaflet({
        #region <- group_by(State, Region) %>%
            #summarise(regionCatProfit)
        
        
        map <- leaflet() %>%
            addTiles () %>%
            setView(0,0,1)
    })

}

    
# Run the application 
shinyApp(ui = ui, server = server)
