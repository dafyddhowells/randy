#' get_prem_fixture_history
#' Calculate rolling averages for goals scored for each team home and away for the same fixture over time.
#' Teams play each other over many seasons and often the outcome follows a certain pattern for e.g. Everton seldom beat Liverpool away.
#' This function returns a breakdown of each team's average points accumulated, goals scored and goals conceded home and away for the same fixture.
#' From this we can assess a team's relative attacking and defending strengths in a given fixture over time to use as variables in a model.
#' @param prem_history a \code{data.frame} of past fixtures given by \code{get_prem_history()}
#' @param home_scored_fixtures_lag The count of lagged games to calculate average home goals scored. Default is 4
#' @param home_conceded_fixtures_lag The count of lagged games to calculate average home goals conceded. Default is 4
#' @param away_scored_fixtures_lag The count of lagged games to calculate average away goals scored. Default is 4
#' @param away_conceded_fixtures_lag The count of lagged games to calculate average away goals conceded. Default is 4
#' @import dplyr
#' @import lubridate
#' @import RcppRoll
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' prem_history <- get_prem_history()
#' prem_fixture_history_df <- get_prem_fixture_history(prem_history)
#' }
#' @return This function returns a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{team}{Character representing a team in a fixture}
#'  \item{fixture}{Character represening the fixture i.e. Arsenal v Everton}
#'  \item{date}{Date when the fixture was played}
#'  \item{fthg}{Numeric fulltime home goals scored in the fixture}
#'  \item{ftag}{Numeric fulltime away goals scored in the fixture}
#'  \item{form}{Numeric running average of points accumulated (3 for a win, 1 for a draw) either home or away}form
#'  \item{sf_scored}{Numeric running average of goals scored either home or away}
#'  \item{sf_conceded}{Numeric running average of goals conceded either home or away}
#'  \item{home}{Numeric logical represnting whether the team was home or away}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

get_prem_fixture_history <- function(prem_history = get_prem_history(),
                                     home_scored_fixtures_lag = 4,
                                     home_conceded_fixtures_lag = 4,
                                     away_scored_fixtures_lag = 4,
                                     away_conceded_fixtures_lag = 4) {

  fixture_history <-

    # Bind home and away same fixtures
    rbind(
      # Home Same Fixture Running Average Scored / Conceded Last n games running
      prem_history %>%
        select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
        arrange(.data$team, .data$fixture) %>%
        cbind(sf_scored = prem_history %>%
                select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
                arrange(.data$team, .data$fixture) %>%
                pull(.data$fthg) %>% roll_mean(n = home_scored_fixtures_lag, align = "right", fill = 0),
              sf_conceded = prem_history %>%
                select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
                arrange(.data$team, .data$fixture) %>%
                pull(.data$ftag) %>% roll_mean(n = home_conceded_fixtures_lag, align = "right", fill = 0)) %>%
        mutate(home = TRUE),

      # Away Same Fixture Running Average Scored / Conceded Last n games running
      prem_history %>%
        select(team = .data$away_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
        arrange(.data$team, .data$fixture) %>%
        cbind(sf_scored = prem_history %>%
                select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
                arrange(.data$team, .data$fixture) %>%
                pull(.data$fthg) %>% roll_mean(n = away_scored_fixtures_lag, align = "right", fill = 0),
              sf_conceded = prem_history %>%
                select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
                arrange(.data$team, .data$fixture) %>%
                pull(.data$fthg) %>% roll_mean(n = away_conceded_fixtures_lag, align = "right", fill = 0)) %>%
        mutate(home = FALSE)
    )


  return(fixture_history)

}
