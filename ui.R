library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "App Store Data (2017)",
    dropdownMenu(
      type = "messages",
      messageItem(
        from = "Author",
        message = "Link to Github.",
        icon = icon("tags"),
        href = "https://github.com/bmetenko",
        time = "2019-06-17 8:50pm"
      )
    ),
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      taskItem(value = 90, color = "green",
               "Git Re-commit"),
      taskItem(value = 75, color = "blue",
               "Server deployment")
    )
  ),
  dashboardSidebar(sidebarMenu(
    menuItem(text = tagList(
      icon("dashboard", lib = "glyphicon"), "Data Plots"
    ),
    tabName = "Plots"),
    menuItem(
      text = tagList(icon("tags", lib = "glyphicon"), "Category Specifics"),
      tabName = "CatPlots"
    ),
    menuItem(text = tagList(
      icon("list-alt", lib = "glyphicon"), "Data Table"
    ), tabName = "Table"),
    menuItem(text = tagList(
      icon("align-left", lib = "glyphicon"), "Data Summary"
    ), tabName = "Summary")
    
    
  )),
  dashboardBody(fluidPage(
    tabItems(
      tabItem(
        tabName = "Plots",
        box(
          plotOutput("plot1"),
          width = "100%",
          status = "success",
          solidHeader = TRUE,
          align = "center",
          title = tagList(icon("cog",
                               lib = "glyphicon"),
                          "App Category Distribution"),
          box(
            uiOutput(outputId = "input1"),
            solidHeader = T,
            title = "Clean?",
            status = "warning"
          ),
          box(uiOutput(outputId = "topCat"), solidHeader = TRUE)
        ),
        box(
          title = tagList(icon("signal",
                               lib = "glyphicon"), "Histogram Distribution"),
          plotOutput("plot2"),
          solidHeader = TRUE,
          width = "100%",
          status = "primary"
        )
        
        
      ),
      tabItem(
        tabName = "Summary",
        h2("App Store Data from 2017"),
        h3("Data source: "),
        h6(
          "https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps"
        ),
        h4(
          "This data set contains more than 7000 Apple iOS mobile application details. /n
          The data was extracted from the iTunes Search API at the Apple Inc website."
        )
      ),
      tabItem(tabName = "CatPlots", fluidPage(box(
        uiOutput("CatPick"),
        box(tableOutput("table3"))
      ), box(
        plotOutput("plot3")
      ))),
      tabItem(tabName = "Table", h2(""), fluidPage(# box(tableOutput("table1"), width = "20%"),
        box(
          tableOutput("table2"), width = "80%"
        )))
    )
  ))
)
