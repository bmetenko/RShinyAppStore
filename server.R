library(shiny)
library(shinydashboard)
library(tidyverse)
library(cowplot)
# library(plotly)

# TODO: try catch on data load. CSV output.


# devtools::install_github("ramamet/applestoreR")
# df <- applestoreR::AppleStore
# write.csv(df, file = "appleData.csv")

df <- read.csv(file = "appleData.csv")

blank_theme <- theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

server <- function(input, output, session) {
  dataParse <- reactive({
    ## Start Reactive wrap
    CatTally <- df %>% group_by(prime_genre) %>% tally()
    
    if (input$naOmit == TRUE) {
      j <- dim(CatTally)[1]
      CatTally <- CatTally[-((input$catNum):24), ]
      CatTally <- CatTally[order(CatTally$n, decreasing = T), ]
      CatTally$prime_genre <-
        factor(x = CatTally$prime_genre,
               levels = CatTally$prime_genre)
      
    } else {
      CatTally <- CatTally[c(1:input$catNum, 24), ]
      CatTally <- CatTally[order(CatTally$n, decreasing = T), ]
      CatTally$prime_genre <-
        factor(x = CatTally$prime_genre,
               levels = CatTally$prime_genre)
    }
    
    CatTally
  })
  
  output$input1 <- renderUI({
    checkboxInput(inputId = "naOmit",
                  label = "Omit NAs?",
                  value = FALSE)
  })
  
  output$topCat <- renderUI({
    sliderInput(
      inputId = "catNum",
      label = "Number of top categories?",
      min = 1,
      max = 23,
      value = 23,
      step = 1
    )
  })
  
  
  output$plot1 <- renderPlot({
    CatTally <- dataParse()
    
    bp <- ggplot(CatTally, aes(
      x = "",
      y = n,
      fill = as.factor(prime_genre)
    )) +
      geom_bar(width = 1,
               color = "white",
               stat = "identity") +
      blank_theme +
      scale_fill_discrete(name = "App Genre")
    # assign(x = "bp", value = bp, envir = .GlobalEnv)
    bp + coord_polar("y", start = 0)
  })
  
  output$plot2 <- renderPlot({
    dataParse()
    
    CatTally <- dataParse()
    
    bp <- ggplot(CatTally,
                 aes(
                   x = CatTally$prime_genre,
                   y = CatTally$n,
                   fill = prime_genre
                 )) +
      geom_bar(stat = "identity") +
      blank_theme + theme(legend.position = "none",
                          axis.text.x = element_text(angle = 45))
    bp
    
  })
  
  output$table1 <- renderTable({
    g <- dataParse()
    
    g
  })
  
  output$table2 <- renderTable({
    head(df, 5)
  })
}
