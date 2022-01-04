#' randy: Data and Functions for Modelling Premier League Goals
#'
#' Source historic premier league data to be used for modelling and predicting the number of goals that each team will score.
#'
#' @docType package
#' @name randy
"_PACKAGE"

# This defines datsets and their columns as global variables to avoid the errors when it comes to check and build
utils::globalVariables(c("prem_2018", "div", "date", "home_team", "away_team", "fthg", "ftag"))
utils::globalVariables(c("prem_2019", "div", "date", "home_team", "away_team", "fthg", "ftag"))
utils::globalVariables(c("prem_2020", "div", "date", "home_team", "away_team", "fthg", "ftag"))
utils::globalVariables(c("prem_pre_2018", "div", "date", "home_team", "away_team", "fthg", "ftag"))
utils::globalVariables(c("ff_results", "home", "away", "actual_score", "kickoff_time", "date_ext"))
utils::globalVariables(c("team", "fthg.y", "ftag.y", "fthg.x", "ftag.x"))
