library(shiny)
library(shinydashboard)
library(tidyverse)
library(cowplot)
library(scales)
library(forcats)
library(ggpubr)
library(sqldf)
library(RColorBrewer)

# source("package_check.R")
# devtools::install_github("ramamet/applestoreR")
# df <- applestoreR::AppleStore
# write.csv(df, file = "appleData.csv")

df <- read.csv(file = "appleData.csv", stringsAsFactors = FALSE)

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
  ### Reactive Context ####
  dataParse <- reactive({
    validate(need(input$naOmit != "", "Waiting for Data..."))
    ## Start Reactive wrap
    CatTally <-
      sqldf("select prime_genre, count(*) from df group by prime_genre order by count(*) desc")
    
    colnames(CatTally)[2] <- "n"
    
    if (input$naOmit == TRUE) {
      CatTally <- CatTally[-1,]
      j <- dim(CatTally)[1]
      CatTally <- CatTally[-((input$catNum + 1):j),]
      CatTally <- CatTally[order(CatTally$n, decreasing = T),]
      CatTally$prime_genre <-
        factor(x = CatTally$prime_genre,
               levels = CatTally$prime_genre)
      
    } else {
      j <- dim(CatTally)[1]
      CatTally <- CatTally[-((input$catNum + 1):j),]
      CatTally <- CatTally[order(CatTally$n, decreasing = T),]
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
  ### CheckBox = Omit NA ####
  output$input1 <- renderUI({
    checkboxInput(inputId = "naOmit",
                  label = "Omit NAs?",
                  value = FALSE)
  })
  
  ### Slider = Category Choice ####
  output$topCat <- renderUI({
    sliderInput(
      inputId = "catNum",
      label = "Number of top categories?",
      min = 2,
      max = 23,
      value = 23,
      step = 1
    )
  })
  
  ### Select = Category ####
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
  
  ### Render = Pie_main ####
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
      scale_fill_discrete(name = "App Genre") +
      {
        if (input$mobileCheck)
          theme(legend.position = "none")
      }
    
    
    
    bp + coord_polar("y", start = 0)
    # assign(x = "bp", value = bp, envir = .GlobalEnv)
    
  })
  
  ### Render = Hist_main ####
  output$plot2 <- renderPlot({
    validate(need(length(dataParse()) != 0, "Loading..."))
    dataParse()
    
    CatTally <- dataParse()
    
    bp <- ggplot(CatTally,
                 aes(
                   x = CatTally$prime_genre,
                   y = CatTally$n,
                   fill = prime_genre
                 )) +
      geom_bar(stat = "identity") +
      geom_label(label = CatTally$n) +
      blank_theme + theme(legend.position = "none",
                          axis.text.x = element_text(angle = 45))
    bp
    
  })
  
  ### Render = Hist - Ratings####
  output$plot4 <- renderCachedPlot({
    validate(need(input$catChoice != "", "Loading..."))
    
    j <- input$catChoice
    k <- df %>% filter(prime_genre == j)
    
    k %>% mutate(rateSummary = fct_infreq(cont_rating)) %>%
      ggplot(aes(fct_rev(cont_rating))) + geom_bar() + coord_flip()
    
    
    g1 <- k %>% count(cont_rating) %>%
      mutate(cont_rating = fct_reorder(cont_rating, n, sum)) %>%
      filter(cont_rating != "NA") %>%
      ggplot(aes(cont_rating, n)) +
      geom_col(aes(fill = cont_rating)) +
      geom_label(aes(label = n), hjust = 1) +
      xlab("Content Age Rating") + ylab("") +
      theme_light() +
      labs(fill = "") + theme(legend.position = "bottom",
                              legend.title.align = 0.5) +
      coord_flip() + scale_fill_brewer(palette = "Oranges")
    
    
    catHist <<- g1
    g1
  }, cacheKeyExpr = {input$catChoice})
  
  ### Render = Pie - Rating ####
  output$plot3 <- renderCachedPlot({
    validate(need(input$catChoice != "", "Loading..."))
    j <- input$catChoice
    # print(j)
    
    k <- df %>% filter(prime_genre == j) %>%
      select(user_rating) %>% group_by(user_rating) %>% tally()
    
    g1 <-
      ggplot(data = k, aes(
        x = "",
        y = n,
        fill = (user_rating)
      )) +
      geom_bar(stat = "identity") + coord_polar("y", start = 0) +
      scale_fill_distiller(direction = 1, palette = "Oranges") + 
      ggtitle(j) + 
      labs(fill = "Rating") +
      blank_theme +
      theme(axis.title.y = element_blank(), axis.text = element_blank())
    
    catPie <<- g1
    g1
  }, cacheKeyExpr = {input$catChoice})
  ### Render = Size ####
  output$plot5 <- renderPlot({
    ### add validate.
    ### add filtering correctly.
    
    dfMBGenre <- df %>% group_by(prime_genre) %>%
      summarize(max = max(size_bytes) / 1000000,
                min = min(size_bytes) / 1000000) %>%
      arrange(desc(max))
    
    dfMBGenre$prime_genre <- fct_inorder(dfMBGenre$prime_genre)
    
    mycolors = c(brewer.pal(name = "Set3", n = 12),
                 brewer.pal(name = "Paired", n = 12))
    
    
    ggplot(data = dfMBGenre) + geom_bar(aes(x = prime_genre,
                                            y = max,
                                            fill = prime_genre),
                                        color = "black",
                                        stat = "identity") +
      geom_bar(
        aes(x = prime_genre,
            y = min),
        fill = "red",
        color = "black",
        stat = "identity"
      ) +
      scale_fill_manual(values = mycolors) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(y = "App Size (MB)", x = "",  fill = "App Genre")  +
      {
        if (input$mobileCheck)
          theme(legend.position = "none")
      } 
  })
  ### Render = UI = Legend_Mobile ####
  
  # Check if needs to exist at all
  output$Legend_Pie_Mobile <- renderUI({
    if (!input$mobileCheck) {
      return()
    } else {
      box(plotOutput(
        outputId = "Pie_Legend",
        width = "auto",
        height = "250px"
      ),
      width = "100%")
    }
  })
  
  # Actually ploting based on above conditions.
  output$Pie_Legend <- renderPlot({
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
      scale_fill_discrete(name = "App Genre") +
      guides(fill = guide_legend(nrow = 8))
    
    # bp + coord_polar("y", start = 0)
    
    bp
    
    get_legend(bp) %>% plot_grid()
    # bp_leg <- as_ggplot(bp_leg)
    # plot_grid(bp_leg)
  })
  ### Table = Full ####
  output$table1 <- renderTable({
    g <- dataParse()
    
    g
  })
  
  ### Table = Full DataTable Tab ####
  output$table2 <- renderDataTable({
    df2 <- df %>% na.omit() %>% select(-c(1, 2, 5, 8, 10, 15, 17))
    colnames(df2) <-  c("App Name", 
                        "Size (MB)", 
                        "Price", 
                        "Rating Count", 
                        "Rating", 
                        "Version", 
                        "Content", 
                        "Genre", 
                        "Supported Devices", 
                        "Language Count")
    df2
  }, options = list(searching = FALSE, 
                    pageLength = 10, 
                    autoWidth = TRUE))
  
  ### Table = pieCheck? ####
  output$pieTable <- renderTable({
    g <- dataParse()
    
    colnames(g) <- c("App Category", "Number of Apps")
    g
  }, align = "c")
  
  
  ### Table = Category ####
  output$table3 <- renderTable({
    validate(need(input$catChoice != "", "Loading..."))
    j <- input$catChoice
    k <- df %>% filter(prime_genre == j) %>%
      select(user_rating) %>% group_by(user_rating) %>% tally()
    
    k$percent <-
      (k$n / sum(k$n)) %>% round(., digits = 2) %>% percent()
    colnames(k) <- c("Rating", "Count", "Percent")
    
    k
  })
  
  ### Data Download ####
  
  output$Pie_Download <-
    downloadHandler(
      filename = function() {
        "AppCatPie.jpeg"
      },
      content = function(file) {
        # ggsave(filename = file, plot = input$plot3())
        ggsave(catPie, filename = file, scale = 5)
      },
      contentType = "image/jpeg"
    )
  
  output$Hist_Download <-
    downloadHandler(
      filename = function() {
        "AppCatHist.jpeg"
      } ,
      content = function(file) {
        ggsave(catHist, filename = file, scale = 5)
      },
      contentType = "image/jpeg"
    )
  ### End of Server ####
}
