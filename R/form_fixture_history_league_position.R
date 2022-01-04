#' form_fixture_history_league_position
#'
#' Joins team averages for goals and form with past fixture history and also calculates a team's league position
#' In addition to the average goals scored and conceded by a team both in recent games and in the same fixture, we would also want to know that's team's league position at the time that the fixture was played.
#' This function joins form and past fixture data frames and calculates the cumulative points for each team after each game within each season.
#' @import dplyr
#' @import jsonlite
#' @import magrittr
#' @import tidyr
#' @import data.table
#' @importFrom rlang .data
#' @param form_and_fixture_history A \code{data.frame} of joined form and fixture history given by \code{join_form_and_fixture_history()}
#' @export
#' @examples
#' \dontrun{
#' df <- join_form_and_fixture_history()
#' form_fixture_history_league_position <- form_fixture_history_league_position(df)
#' }
#' @return This function returns a \code{data.frame} with the following columns:
#' \itemize{
#'  \item{team}{Character values for the team}
#'  \item{fixture}{Character represening the fixture i.e. Arsenal v Everton}
#'  \item{date}{Character values for the date that the match was played}
#'  \item{fthg}{Numeric fulltime home goals scored in the fixture}
#'  \item{ftag}{Numeric fulltime away goals scored in the fixture}ftag
#'  \item{form}{Numeric running average of points accumulated (3 for a win, 1 for a draw) either home or away}form
#'  \item{scored}{Numeric running average of goals scored either home or away}
#'  \item{conceded}{Numeric running average of goals conceded either home or away}
#'  \item{home}{Numeric logical represnting whether the team was home or away}
#'  \item{win}{Numeric logical represnting whether the team won the fixture}
#'  \item{draw}{Numeric logical represnting whether the team drew the fixture}
#'  \item{sf_scored}{Numeric running average of goals scored either home or away}
#'  \item{sf_conceded}{Numeric running average of goals conceded either home or away}
#'  \item{season}{Character representing the premier league season}
#'  \item{points}{Numeric representing the points gained by the team in the fixture}
#'  \item{cum_points}{Numeric representing the cumulative points gained by the team after n games in the season}
#'  \item{game}{Numeric representing the game count. A team will play 38 games in the modern era of the premier league}
#'  \item{league_pos}{Numeric representing a team's league position after n games in a season}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

form_fixture_history_league_position <- function(form_and_fixture_history = join_form_and_fixture_history()){

  league_pos_changes <- list()

  prem_teams <- join_form_and_fixture_history() %>% distinct(.data$team) %>% pull(.data$team)

  form_and_fixture_history <-
    form_and_fixture_history %>% append_seasons()

  for(i in 1:length(prem_teams)) {

    league_pos_changes[[i]] <- form_and_fixture_history %>%
      filter(.data$team == prem_teams[i]) %>%
      arrange(.data$date) %>%
      mutate(points = case_when(
        win == 1 ~ 3,
        #win == 0 ~ 0,
        draw == 1 ~ 1,
        #draw == 0 ~ 0
      )) %>%
      replace_na(list(points = 0)) %>%
      group_by(.data$team, .data$season) %>%
      mutate(cum_points = cumsum(.data$points),
             game = row_number())

  }

  form_with_past_fixtures_plus_league_pos <- rbindlist(league_pos_changes,
                                                                   use.names = TRUE) %>%
    ungroup() %>%
    arrange(.data$date, .data$season) %>%
    group_by(.data$season, .data$game) %>%
    mutate(league_pos = dense_rank(desc(.data$cum_points)))

  return(form_with_past_fixtures_plus_league_pos)

}
