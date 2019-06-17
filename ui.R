library(shiny)
library(shinydashboard)


ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "App Store Data (2017)"),
  dashboardSidebar(sidebarMenu(
    menuItem(text = "Data Summary", tabName = "Summary"),
    menuItem(text = "Data Table", tabName = "Table"),
    menuItem(text = "Data Plots", tabName = "Plots")
  )),
  dashboardBody(fluidPage(
    box(
      plotOutput("plot1"),
      width = "100%",
      status = "success",
      solidHeader = TRUE,
      align = "center",
      title = "App Category Distribution",
      box(
        uiOutput(outputId = "input1"),
        solidHeader = T,
        title = "Clean?",
        status = "warning"
      ),
      box(uiOutput(outputId = "topCat"))
    )
    
  ))
)
