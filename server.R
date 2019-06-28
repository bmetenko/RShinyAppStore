library(shiny)
library(shinydashboard)
library(tidyverse)
library(cowplot)
library(scales)
library(forcats)

# TODO: Add Download Buttons.
# TODO: Travis CI.
# TODO: Tab box convert.
# TODO: Update person statement in UI.
# TODO: Scatterplots and ggplotly output.

# source("package_check.R")
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
    validate(need(input$naOmit != "", "Waiting for Data..."))
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
    
    if (is.null(CatTally)) {
      return()
    } else {
      CatTally
      
    }
    
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
    validate(need(length(dataParse()) != 0, "Loading..."))
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
    
    
    
    bp + coord_polar("y", start = 0)
    # assign(x = "bp", value = bp, envir = .GlobalEnv)
    
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
  
  output$table2 <- renderDataTable({
    head(df[, 1:15] %>% as.tibble(), 25)
  })
  
  output$CatPick <- renderUI({
    d <- unique(df$prime_genre)
    
    selectInput(
      inputId = "catChoice",
      label = "Please Select Category",
      choices = d,
      selected = d[1],
      width = "100%"
    )
  })
  
  output$plot3 <- renderPlot({
    validate(need(input$catChoice != "", "Loading..."))
    j <- input$catChoice
    print(j)
    
    k <- df %>% filter(prime_genre == j) %>%
      select(user_rating) %>% group_by(user_rating) %>% tally()
    
    g1 <-
      ggplot(data = k, aes(
        x = "",
        y = n,
        fill = as.factor(user_rating)
      )) +
      geom_bar(stat = "identity") + coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Spectral") + ggtitle(j) + labs(fill = "Rating") +
      blank_theme +
      theme(axis.title.y = element_blank())
    g1
  })
  
  output$table3 <- renderTable({
    validate(need(input$catChoice != "", "Loading..."))
    j <- input$catChoice
    k <- df %>% filter(prime_genre == j) %>%
      select(user_rating) %>% group_by(user_rating) %>% tally()
    
    k$percent <-
      (k$n / sum(k$n)) %>% round(., digits = 2) %>% percent()
    k
  })
  
  output$plot4 <- renderPlot({
    validate(need(input$catChoice != "", "Loading..."))
    
    j <- input$catChoice
    k <- df %>% filter(prime_genre == j)
    
    k %>% mutate(rateSummary = fct_infreq(cont_rating)) %>%
      ggplot(aes(fct_rev(cont_rating))) + geom_bar() + coord_flip()
    
    
    k %>% count(cont_rating) %>% mutate(cont_rating = fct_reorder(cont_rating, n, sum)) %>% filter(cont_rating != "NA") %>%
      ggplot(aes(cont_rating, n)) + geom_col(aes(fill = cont_rating)) + coord_flip()
    
  })
}
