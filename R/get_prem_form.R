#' Get rolling averages for points accumulated and goals scored for each team home and away.
#'
#' This functions returns a breakdown of each team's average points accumulated, goals scored and goals conceded home and away.
#'
#' @param prem_history a \code{data.frame} of past fixtures given by \code{get_prem_history()}
#' @param home_form_games_lag The count of lagged games to calculate average home points. Default is 4
#' @param home_scored_games_lag The count of lagged games to calculate average home goals scored. Default is 4,
#' @param home_conceded_games_lag The count of lagged games to calculate average home goals conceded. Default is 4,
#' @param away_form_games_lag The count of lagged games to calculate average away points. Default is 4,
#' @param away_scored_games_lag The count of lagged games to calculate average away goals scored. Default is 4,
#' @param away_conceded_games_lag The count of lagged games to calculate average away goals conceded Default is 4,
#' @import dplyr
#' @import lubridate
#' @import RcppRoll
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom tidyr replace_na
#' @export
#' @examples
#' \dontrun{
#' prem_history <- get_prem_history()
#' prem_form_df <- get_prem_form(prem_history)
#' }
#' @return This function returns a \code{data.frame} with the following columns:
#' \describe{
#'  \item{team}{Character representing a team in a fixture}
#'  \item{fixture}{Character represening the fixture i.e. Arsenal v Everton}
#'  \item{date}{Date when the fixture was played}
#'  \item{fthg}{Numeric fulltime home goals scored in the fixture}
#'  \item{ftag}{Numeric fulltime away goals scored in the fixture}
#'  \item{form}{Numeric running average of points accumulated (3 for a win, 1 for a draw) either home or away}
#'  \item{scored}{Numeric running average of goals scored either home or away}
#'  \item{conceded}{Numeric running average of goals conceded either home or away}
#'  \item{home}{Numeric logical represnting whether the team was home or away}
#'  \item{win}{Numeric logical represnting whether the team won the fixture}
#'  \item{draw}{Numeric logical represnting whether the team drew the fixture}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

get_prem_form <- function(prem_history = get_prem_history(),
                          home_form_games_lag = 4,
                          home_scored_games_lag = 4,
                          home_conceded_games_lag = 4,
                          away_form_games_lag = 4,
                          away_scored_games_lag = 4,
                          away_conceded_games_lag = 4) {

  form <- rbind(

    # Home form
    cbind(prem_history %>%
            select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(team, date),

          form = prem_history %>%
            mutate(result = case_when(.data$fthg > .data$ftag ~ 3,
                                      .data$fthg < .data$ftag ~ 0,
                                      .data$fthg == .data$ftag ~ 1)) %>%
            select(team = .data$home_team, .data$fixture, .data$result, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date) %>%

            pull(.data$result) %>%

            roll_mean(n = home_form_games_lag, align = "right", fill = NA),

          # Home scored

          scored =
            prem_history %>%
            select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date) %>%
            pull(.data$fthg) %>% roll_mean(n = home_scored_games_lag, align = "right", fill = 0),

          # Home conceded

          conceded = prem_history %>%
            select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date) %>%
            pull(.data$ftag) %>% roll_mean(n = home_conceded_games_lag, align = "right", fill = 0)

    ) %>% mutate(home = TRUE) %>%
      mutate(win = ifelse(.data$fthg > .data$ftag, 1, 0),
             draw = ifelse(.data$ftag == .data$fthg, 1, 0)),

    # Away form

    cbind(prem_history %>%
            select(team = .data$away_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date),

          form = prem_history %>%
            mutate(result = case_when(.data$fthg < .data$ftag ~ 3,
                                      .data$fthg > .data$ftag ~ 0,
                                      .data$fthg == .data$ftag ~ 1)) %>%
            select(team = .data$home_team, .data$fixture, .data$result, .data$date, .data$fthg, .data$ftag) %>%
            arrange(team, date) %>%

            pull(.data$result) %>%

            roll_mean(n = away_form_games_lag, align = "right", fill = NA),

          # Away Scored

          scored = prem_history %>%
            select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date) %>%
            pull(.data$ftag) %>% roll_mean(n = away_scored_games_lag, align = "right", fill = 0),

          # Away Conceded

          conceded = prem_history %>%
            select(team = .data$home_team, .data$fixture, .data$date, .data$fthg, .data$ftag) %>%
            arrange(.data$team, .data$date) %>%
            pull(.data$fthg) %>% roll_mean(n = away_conceded_games_lag, align = "right", fill = 0)

    ) %>% mutate(home = FALSE) %>%
      mutate(win = ifelse(.data$fthg < .data$ftag, 1, 0),
             draw = ifelse(.data$ftag == .data$fthg, 1, 0))
  ) %>%
    replace_na(list(form = 0))

  return(form)

}
