#' get_model_data
#'
#' Joins team averages for goals and form, past fixture history, league position in season and xG averages in recent games.
#' This function generates a data frame with a number of variables to represent a team's attacking and defending strength.
#' This is based on recent games and also past history of each fixture.
#' @import dplyr
#' @import jsonlite
#' @importFrom tidyr separate
#' @importFrom tidyr replace_na
#' @importFrom tidyr drop_na
#' @importFrom magrittr "%>%"
#' @param x A \code{data.frame} of joined form and fixture history given by \code{form_fixture_history_league_position()}
#' @export
#' @examples
#' \dontrun{
#' model_data <- get_model_data()
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

get_model_data <- function(x = form_fixture_history_league_position()) {

  df <- x %>%
    left_join(get_xg_fixture_history(),
              by = c("team" = "team",
                     "date" = "date",
                     "fixture" = "fixture",
                     "home" = "home")) %>%
    arrange(desc(date))

  return(df)

}
