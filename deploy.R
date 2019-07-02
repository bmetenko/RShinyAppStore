library(rsconnect)
 # source("package_check.R")

setAccountInfo(name = Sys.getenv("shinyapps_name"),
               token = Sys.getenv("shinyapps_token"),
               secret = Sys.getenv("shinyapps_secret"))

deployApp()