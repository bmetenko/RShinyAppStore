library(shiny)
library(shinydashboard)

#### HEADER ####
Main_Header <- dashboardHeader(
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
)

#### SIDEBAR ####
Main_Sidebar <- dashboardSidebar(sidebarMenu(
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
  
  
))

#### Modifications #####
Box_Clean <- box(width = "50%", 
                 height = "100px",
                 align = "center",
                 uiOutput(outputId = "input1"),
                 solidHeader = T,
                 title = "Clean?",
                 status = "warning"
)

Box_Category <- box(width = "50%", 
                    height = "100px",
                    uiOutput(outputId = "topCat"), 
                    status = "warning",
                    solidHeader = TRUE)

#### Box with plots ####
BoxTop_Pie_Plot1 <- box(
  plotOutput("plot1"),
  width = "100%",
  status = "success",
  solidHeader = TRUE,
  align = "center",
  title = tagList(icon("cog",
                       lib = "glyphicon"),
                  "App Category Distribution")
)


BoxTop_Hist_Plot2 <- box(
  title = tagList(icon("signal",
                       lib = "glyphicon"), 
                  "Histogram Distribution"),
  plotOutput("plot2"),
  solidHeader = TRUE,
  width = "100%",
  status = "primary"
)

Box_CatTable <- box(
  solidHeader = T,
  title = "Categorical Analysis",
  status = "info",
  uiOutput("CatPick"),
  tableOutput("table3"),
  align = "center",
  width = "100px"
)

Box_CatPie_Plot3 <- box(plotOutput("plot3"))

Box_CatHist_Plot4 <- box(plotOutput("plot4"))

BoxTop_Placeholder <- box(title = "Work In Progress", 
                          status = "warning", 
                          solidHeader = T,
                          width = "100%",
                          plotOutput("plot5"))

#### Information Tab ####
Tab_Info <- tabItem(
  tabName = "Summary",
  h2("App Store Data from 2017"),
  h3("Data source: "),
  h6("https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps"),
  h4("This data set contains more than 7000 Apple iOS mobile application details.
     The data was extracted from the iTunes Search API at the Apple Inc website."))

#### Full Table Tab ####
Tab_CatFull_Table <-  box(dataTableOutput("table2"), width = "50%")

#### UI DECLARATION ####
ui <- dashboardPage(skin = "black",
                    Main_Header,
                    Main_Sidebar,
                    dashboardBody(fluidPage(
                      tabItems(
                        tabItem(
                          tabName = "Plots",
                          # BoxTop_Pie_Plot1,
                          # BoxTop_Hist_Plot2,
                          fluidRow(column(6,Box_Clean), 
                                   column(6, Box_Category)),
                          tabBox(id = "TabBox1", width = "100%",
                                 selected = "Pie Chart",
                                 tabPanel("Pie Chart", BoxTop_Pie_Plot1), 
                                 tabPanel("Histogram", BoxTop_Hist_Plot2),
                                 tabPanel("Storage", BoxTop_Placeholder)
                          )
                          
                        ),
                        Tab_Info,
                        tabItem(tabName = "CatPlots", fluidPage(
                          Box_CatTable,
                          Box_CatPie_Plot3,
                          Box_CatHist_Plot4
                        )),
                        tabItem(tabName = "Table", fluidPage(
                          h2(""), 
                          Tab_CatFull_Table)
                        )))))



#### Todo list ####
# TODO: Add Download Buttons.
# TODO: Sizes_min_max vs category. vs. user rating.
# TODO: Column (6) main tabs.
# TODO: Tab box convert.
# TODO: Update person statement in UI.
# TODO: Scatterplots and ggplotly output.
# TODO: Clean up code.
# TODO: Start on Powerpoint or Xaringan.
# TODO: Unit tests.
# TODO: Aside: NASA project.