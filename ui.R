library(shiny)
library(shinydashboard)
library(rpivotTable)

#### HEADER ####
Main_Header <- dashboardHeader(
  title = "App Store Data (2017)",
  dropdownMenu(
    type = "tasks",
    badgeStatus = "success",
    taskItem(value = 50, color = "yellow",
             "Code Refactoring"),
    taskItem(value = 90, color = "orange",
             "Git Re-commit"),
    taskItem(value = 95, color = "green",
             "Server deployment"),
    taskItem(
      value = 100,
      color = "blue",
      "Animated Gradient Background"
    )
  ),
  dropdownMenu(
    type = "messages",
    messageItem(
      from = "Author",
      message = "Link to Github.",
      icon = icon("tags"),
      href = "https://github.com/bmetenko",
      time = "2019-06-17 8:50pm"
    )
  )
)

#### SIDEBAR ####
Main_Sidebar <- dashboardSidebar(
  #### MAIN CSS ####
  tags$link(rel = "stylesheet", 
            type = "text/css", 
            href = "main.css"),
  #### Sidebar Menu ####
  sidebarMenu(
    menuItem(text = tagList(
      icon("dashboard", lib = "glyphicon"),
      " Data Plots"
    ),
    tabName = "Plots"),
    menuItem(
      text = tagList(icon("tags", lib = "glyphicon"), " Category Specifics"),
      tabName = "CatPlots"
    ),
    menuItem(text = tagList(
      icon("list-alt", lib = "glyphicon"), " Data Table"
    ), tabName = "Table"),
    menuItem(text = tagList(
      icon("align-left", lib = "glyphicon"), " Data Summary"
    ), tabName = "Summary"),
    tags$img(
      class = "hex-svg",
      src = "hex2.svg", 
      align = "center",
      style = "display: block; 
      margin-left: auto; 
      margin-right: auto;",
      width = "75%"
    )
    
    
  )
)

#### Plain HTML ####

info_Title <- HTML(
  '<h2 
  class = "info-title">Information</h2>'
)

paragraph_format <- function(str) {
  tmp <- paste0(
    '<h5 style="color: black;
    background-color: #3c8dbc;
    text-align: center;
    padding: 15px;
    border-radius: 10px;">',
    str,
    '</h5>'
  )
  tmp2 <- HTML(tmp)
  
  return(tmp2)
}

title_format <- function(str, ico) {
  tags$body(align = "center", tagList(icon(ico,
                                           lib = "glyphicon"),
                                      str))
}



### Background CSS maybe?

#### Text Sources ####

Pie_Explain <-
  "This categorical distribution visualization can be used by stakeholders to identify specific app markets that are less inundated by apps than others, as well as help plan out how much competition there is in this space for future app development. Popularity of specific app genres can also be avaluated for consideration."

Hist_Explain <-
  "This categorical distribution histogram more clearly and cleanly defines the quantity of apps found for each category on the iPhone app store in 2017. It can be used similarly in discussion as the pie chart visualization in the previous tab."

Size_Explain <-
  "Data on the app size differences can be used as a proxy for development time involved in publishing and maintaining a specific type of app. Size data can be a rough proxy for how much money is necessary for these actions as well."

#### Data Download ####
Down_Cat_Pie <-
  downloadButton(label = "Download Plot", outputId = "Pie_Download")
Down_Cat_Hist <-
  downloadButton(label = "Download Plot", outputId = "Hist_Download")

#### Modifications #####
Box_Clean <- box(
  width = "50%",
  height = "100px",
  align = "center",
  uiOutput(outputId = "input1",
           # For Tailwind CSS Styling > CheckboxOutput
           class = ""),
  solidHeader = T,
  title = "Clean?",
  status = "warning",
  # For Tailwind CSS styling > BOX
  class = ""
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
BoxTop_Pie_Plot1 <- tagList(
  box(
    plotOutput("plot1"),
    width = "100%",
    status = "primary",
    solidHeader = TRUE,
    align = "center",
    title = title_format("App Category Distribution", "cog")
  )
)


BoxTop_Hist_Plot2 <- box(
  title = title_format("Histogram Distribution", "signal"),
  plotOutput("plot2"),
  solidHeader = TRUE,
  width = "100%",
  status = "primary"
)

Box_CatTable <- box(
  solidHeader = T,
  collapsed = F,
  collapsible = T,
  title = title_format("Categorical Analysis", "tasks"),
  status = "primary",
  uiOutput("CatPick"),
  tableOutput("table3"),
  align = "center",
  width = "100%"
)

Box_CatPie_Plot3 <-
  box(
    plotOutput("plot3"),
    (Down_Cat_Pie),
    title = "Pie Chart",
    align = "center",
    collapsible = T,
    status = "primary",
    solidHeader = T,
    width = "50%"
  )

Box_CatHist_Plot4 <-
  box(
    plotOutput("plot4"),
    (Down_Cat_Hist),
    title = "Bar Chart",
    align = "center",
    collapsible = T,
    status = "primary",
    solidHeader = T,
    width = "50%"
  )

BoxTop_Size <- box(
  title = tagList(icon("export",
                       lib = "glyphicon"),
                  "App Size by Category"),
  status = "primary",
  solidHeader = T,
  width = "100%",
  plotOutput("plot5")
)

BoxTop_Pie_Legend <- uiOutput("Legend_Pie_Mobile", width = "auto")

#### Information Tab ####
Tab_Info <- tabItem(
  tabName = "Summary",
  HTML('<a href= "https://github.com/bmetenko/RShinyAppStore">
       <img src = "https://travis-ci.org/bmetenko/RShinyAppStore.svg?branch=master">
       </a>'),
  h2("App Store Data from 2017"),
  h3("Data source: "),

  h6(
    "https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps"
  ),
  h4(
    "This data set contains more than 7000 Apple iOS mobile application details.
    The data was extracted from the iTunes Search API at the Apple Inc website."
  ),
  h3("Author Note:"),
  h4(
    "This Shiny application was built as an effort to enhance my skills with R Shiny and R Shiny dashboards, as well as to understand the technology behind continuous integration / continuous deployment (CI/CD) using Travis CI and Github. The current version of the App Store on iOS or iPadOS does not allow for easy interpretation of the categories and properties of apps available and so this application was made in an effort to better portray this information."
  )
)


#### Data Tables ####
pieTable <- box(
  id = "tablePie",
  tableOutput('pieTable'),
  width = "100%",
  solidHeader = TRUE,
  collapsed = TRUE,
  title = "Data Table",
  collapsible = TRUE
)


#### Button Toggle ####

#### Full Table Tab ####
Tab_CatFull_Table <-
  box(dataTableOutput("table2"),
      width = "100%",
      align = "center")

#### KPI Container ####

KPI_Container <- tagList(
  valueBox(
    value = 299.99,
    width = 4,
    icon = icon("usd", lib = "glyphicon"),
    subtitle = "Most expensive app cost (USD)"
  ),
  valueBox(
    value = 0.99,
    width = 4,
    icon = icon("usd", lib = "glyphicon"),
    subtitle = "Most likey app cost (USD)"
  ),
  valueBox(
    value = 7907,
    width = 4,
    icon = icon("barcode", lib = "glyphicon"),
    "Labeled apps in dataset"
  )
)

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
                              fluidPage(BoxTop_Size,
                                        info_Title,
                                        paragraph_format(Size_Explain))
                            )
                          ),
                          fluidRow(align = "center",
                                   column(
                                     align = "center",
                                     ## Maybe implement uioutput switch statement based on
                                     ## the current tab you're on.
                                     pieTable, width = 12
                                   )),
                          KPI_Container
                        ),
                        Tab_Info,
                        tabItem(
                          tabName = "CatPlots",
                          fluidRow(Box_CatTable),
                          fluidRow(Box_CatPie_Plot3,
                                   Box_CatHist_Plot4)
                        ),
                        tabItem(tabName = "Table",
                                fluidRow(box(title = "Full Data Table (x/y scrollable)", 
                                             width = "100%",
                                             collapsible = T,
                                             collapsed = FALSE, 
                                             Tab_CatFull_Table, id = "full_data_table"
                                )),
                        fluidRow(box(collapsible = T, 
                                     collapsed = T, 
                                     width = "100%",
                                     title = "Pivot Table (x/y scrollable)",
                                     # Perhaps add warning here, or check if mobile and not render.
                          rpivotTableOutput("pivotC", width = "100%"))
                        ))
                      )
                    )))



#### Todo list ####
# TODO: Sizes_min_max vs category. vs. user rating.
# TODO: Scatterplots and ggplotly output.
# TODO: Clean up code. ShinyWidgets.
# TODO: Start on Powerpoint or Xaringan.
# TODO: Unit tests.
# TODO: Plot Cache.
# TODO: Write out naming conventions.
# TODO: Comment everywhere. Check if comments here and on ubuntu/master are the same.
# TODO: CSS comments multiline via '/* ========= text ========= */' and
##      table of contents?