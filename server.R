library(shiny)
library(shinydashboard)

# TODO: try catch on data load. CSV output.
# devtools::install_github("ramamet/applestoreR")
df <- applestoreR::AppleStore

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    priceTally <- df %>% group_by(input$cat) %>% tally() 
    
    pie(x = priceTally[,2], labels = priceTally[,1])
  })
  
  output$input1 <- renderUI({
    ch <- df[, sapply(df, class) == "numeric"] %>% colnames()
    selectInput(inputId = "cat", choices = ch, label = "Category")
  })
}

