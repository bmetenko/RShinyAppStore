library(shiny)
library(shinydashboard)


ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "App Store Data (2017)"),
  dashboardSidebar(sidebarMenu(
    menuItem(text = "Data Summary"),
    menuItem(text = "Data Table"),
    menuItem(text = "Data Plots")
  )),
  dashboardBody()
)