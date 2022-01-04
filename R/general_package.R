

#library(usethis)
#library(RcppRoll)
#library(rio)
#library(tidyverse)
#library(janitor)
#library(jsonlite)
#library(lubridate)
#library(magrittr)
#library(dplyr)


# Add data
#usethis::use_data(prem_pre_2018, overwrite = TRUE)
#usethis::use_data(prem_2020, overwrite = TRUE)
#usethis::use_data(prem_2019, overwrite = TRUE)
#usethis::use_data(prem_2018, overwrite = TRUE)

# Create vignette
#use_vignette("Randy")

# Write a function to a R directory
#dump("name of function in quotes", file = "datasummary/R/data_summary.R")

# roxygen is package that generates documentation
# You put a header at the top of the function definition that looks like this below
# The first three lines of the header have special meaning
#   and you don't need to use tags to identify them. The first three lines are:

#title
#description
#details


#' Title of the function
#'
#' Description of what it does
#' @param x the description of the parameter
#' @param y the description of the parameter
#' @import dplyr #Import dplyr
#' @import purrr # Import purrr
#' @importFrom tidyr gather Import 1 function from a package
#' @export # This exports the function so it is available to end users

# This roxygen header goes above the function definition

# A utility function works within a function and you may not want to make this
#   available to end users.
# Exported and non exported functions are ones that you do and do not want to
#   make available to end users


