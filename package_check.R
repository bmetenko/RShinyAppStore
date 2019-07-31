required_pkg = c("shiny", "ggplot2", 
                 "dplyr","jpeg", 
                 "shinydashboard", "httr", 
                 "forcats", "jsonlite", 
                 "sqldf", "RColorBrewer", 
                 "ggpubr",
                 "scales", "cowplot",
                 "tidyverse")

packages_to_add = required_pkg[!(required_pkg %in% installed.packages()[, 1])]

if (length(packages_to_add) > 0) {
  install.packages(packages_to_add,dependencies = T, repos = "https://cran.rstudio.com")
}
