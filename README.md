# Camino-Project-3
St558 Project 3
Author: Steph Camino

## Purpose: 

In this repo you will find code for this project in the app.R file. This code creates a shiny app that is used to explore data and model it. 

## R packages used:  

* [`shiny`](https://shiny.rstudio.com/): This package is used to build the interactive web app. 
* [`shinydashboard`](https://rstudio.github.io/shinydashboard/): This package is used to create dashboards, specifically for the tabs and different pages. 
* [`fontawesome`](https://cran.r-project.org/web/packages/fontawesome/index.html): This package is used to make the fun icons for the tabs
* [`readxl`](): This package is used to read in the data from an excel file.
* [`lubridate`](): This package is used to convert a character into a date in the data. 
* [`knitr`](): This package is used to greate nicely printed tables.
* [`TidyVerse`](https://www.tidyverse.org/): This package was loaded to retrieve the packages below.
  * [`ggplot2`](https://ggplot2.tidyverse.org/): This package was used to create our plots for the exploratory data analysis.
  * [`dplyr`](https://dplyr.tidyverse.org/): This package was used to `select`, `filter`, and `summarise` our data. 



  * [`purrr`](https://purrr.tidyverse.org/): This package was used to map over multiple inputs simultaneously.
  * [`tibble`](https://tibble.tidyverse.org/): This package was used to change the data into a tibble without simplifying the data.. 

## Code to install packages:

`install.packages(c("shiny", "shinydashboard", "fontawesome", "readxl", "lubridate", "knitr", "tidyverse"))`

## Code to create app:
  
`shiny::runGitHub("Camino-Project-3", "smcamino")`

