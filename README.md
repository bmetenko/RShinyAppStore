# RShinyAppStore

[![Build Status](https://travis-ci.org/bmetenko/RShinyAppStore.svg?branch=master)](https://travis-ci.org/bmetenko/RShinyAppStore)


A simple shiny dashboard to quickly evaluate Apple App Store Data from 2017.
It is available for view in its most current successful build state at:
> https://bmetenko.shinyapps.io/RShinyAppStore/

This Shiny application was built as an effort to enhance the author's skills with R Shiny and R Shiny dashboards, as well as to understand the technology behind continuous integration / continuous deployment (CI/CD) using Travis CI and Github. The current version of the App Store on iOS or iPadOS does not allow for easy interpretation of the categories and properties of apps available and so this application was made in an effort to better portray this information.

Data used is available freely at:

  >_https://www.kaggle.com/ramamet4/app-store-apple-data-set-10k-apps_
  
It can also be accessed in R through:

  >`devtools::install_github("ramamet/applestoreR")`
  >`applestoreR::AppleStore`
 
It was originally extracted by use of an iTunes Search API by
(Copyright (c) 2018 Ramanathan Perumal) and is usable under its GPL 2 license.
