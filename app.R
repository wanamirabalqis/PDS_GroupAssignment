library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("salesbcll.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
    titlePanel("Estimates of Monthly Retail and Food Services Sales by Kind of Business"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("SalesInput", "Sales", min = 1,max = 20000,value = c(3000,8000), pre = "$"),
            radioButtons("ProductInput", "Product Type",
                         choices = c("Furniture", "Technology", "Office Supplies"),
                         selected = "Furniture"),
            uiOutput("YearOutput")
        ),
        mainPanel(
            plotOutput("coolplot"),
            br(), br(),
            tableOutput("results")
        )
    )
)
server <- function(input, output) {
    output$YearOutput <- renderUI({
        selectInput("YearInput", "Year",
                    sort(unique(bcl$Year)),
                    selected = "2008")
    })  
    
    filtered <- reactive({
        if (is.null(input$YearInput)) {
            return(NULL)
        }    
        
        bcl %>%
            filter(Sales >= input$SalesInput[1],
                   Sales <= input$SalesInput[2],
                   Product == input$ProductInput,
                   Year == input$YearInput
            )
    })
    output$coolplot <- renderPlot({
        if (is.null(input$YearInput())) {
            return(NULL)
        }
        ggplot(filtered(), aes(input$SalesInput)) +
            geom_histogram()
    })
    
    output$results <- renderTable({
        filtered()
    })
} 

shinyApp(ui = ui, server = server)