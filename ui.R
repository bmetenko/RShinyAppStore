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

#### Plain HTML ####

info_Title <- HTML(
  '<h2 style="color: #7b94a4;
  background-color: #bdcdc5;
  text-shadow: 3px 2px #dee6de;
  text-align: center;
  border-radius: 25px;">Information</h2>'
)

paragraph_format <- function(str) {
  tmp <- paste0(
    '<h5 style="color: #393939;
    background-color: #ffac6a;
    text-align: center;
    padding: 15px;
    border-radius: 10px;">',
    str,
    '</h5>'
  )
  tmp2 <- HTML(tmp)
  
  return(tmp2)
}

#### Text Sources ####

Pie_Explain <-
  "This categorical distribution visualization can be used by stakeholders to identify specific app markets that are less inundated by apps than others, as well as help plan out how much competition there is in this space for future app development. Popularity of specific app genres can also be avaluated for consideration."

Hist_Explain <-
  "This categorical distribution histogram more clearly and cleanly defines the quantity of apps found for each category on the iPhone app store in 2017. It can be used similarly in discussion as the pie chart visualization in the previous tab."

Size_Explain <-
  "Data on the app size differences can be used as a proxy for development time involved in publishing and maintaining a specific type of app. Size data can be a rough proxy for how much money is necessary for these actions as well."


#### Modifications #####
Box_Clean <- box(
  width = "50%",
  height = "100px",
  align = "center",
  uiOutput(outputId = "input1"),
  solidHeader = T,
  title = "Clean?",
  status = "warning"
)

Box_Category <- box(
  width = "50%",
  height = "100px",
  uiOutput(outputId = "topCat"),
  status = "warning",
  solidHeader = TRUE
)
Box_Mobile <- box(
  width = "50%",
  height = "100px",
  status = "warning",
  align = "center",
  checkboxInput("mobileCheck",
                label = "Move Legends?",
                value = FALSE),
  solidHeader = TRUE,
  title = "Mobile?"
)

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

BoxTop_Placeholder <- box(
  title = "App Size by Category",
  status = "primary",
  solidHeader = T,
  width = "100%",
  plotOutput("plot5")
)

BoxTop_Pie_Legend <- uiOutput("Legend_Pie_Mobile", width = "auto")

#### Information Tab ####
Tab_Info <- tabItem(
  tabName = "Summary",
  h2("App Store Data from 2017"),
  h3("Data source: "),
  h6(
    "https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps"
  ),
  h4(
    "This data set contains more than 7000 Apple iOS mobile application details.
    The data was extracted from the iTunes Search API at the Apple Inc website."
  )
)

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
                          fluidRow(
                            column(4, Box_Clean),
                            column(4, Box_Category),
                            column(4, Box_Mobile)
                          ),
                          tabBox(
                            id = "TabBox1",
                            width = "100%",
                            selected = "Pie Chart",
                            tabPanel(
                              "Pie Chart",
                              fluidPage(
                                BoxTop_Pie_Plot1,
                                info_Title,
                                paragraph_format(Pie_Explain),
                                BoxTop_Pie_Legend
                              )
                            ),
                            tabPanel(
                              "Histogram",
                              fluidPage(
                                BoxTop_Hist_Plot2,
                                info_Title,
                                paragraph_format(Hist_Explain)
                              )
                            ),
                            tabPanel(
                              "Storage",
                              fluidPage(
                                BoxTop_Placeholder,
                                info_Title,
                                paragraph_format(Size_Explain)
                              )
                            )
                          )
                          
                        ),
                        Tab_Info,
                        tabItem(
                          tabName = "CatPlots",
                          fluidPage(Box_CatTable),
                          fluidRow(Box_CatPie_Plot3, Box_CatHist_Plot4)
                        ),
                        tabItem(tabName = "Table", fluidPage(h2(""),
                                                             Tab_CatFull_Table))
                      )
                    )))



#### Todo list ####
# TODO: Add Download Buttons.
# TODO: Sizes_min_max vs category. vs. user rating.
# TODO: Column (4) main tabs. Rows?
# TODO: Tab box convert. Fix histogram and ordering of the
# TODO: Update person statement in UI.
# TODO: Scatterplots and ggplotly output.
# TODO: Clean up code.
# TODO: Start on Powerpoint or Xaringan.
# TODO: Unit tests.
# TODO: Legend output differently.
# TODO: Aside: NASA project.