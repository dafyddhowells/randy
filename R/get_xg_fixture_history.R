#' Calculate rolling averages expected goals (xG) scored for each team home and away
#'
#' This function returns a breakdown of each team's rolling average xG for and against both home and away.
#'
#' @param home_scored_games_lag The count of lagged games to calculate average home xG. Default is 4,
#' @param home_conceded_games_lag The count of lagged games to calculate average home xG against. Default is 4,
#' @param away_scored_games_lag The count of lagged games to calculate average away xG. Default is 4,
#' @param away_conceded_games_lag The count of lagged games to calculate average away xG against. Default is 4,
#' @import dplyr
#' @import lubridate
#' @import RcppRoll
#' @import rvest
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom data.table rbindlist
#' @export
#' @examples
#' \dontrun{
#' xg_fixture_history_df <- get_xg_fixture_history()
#' }
#' @return This function returns a data.frame with the following columns
#' \describe{
#'  \item{date}{Date when the fixture was played}
#'  \item{team}{Character representing a team in a fixture}
#'  \item{fixture}{Character represening the fixture e.g Arsenal v Everton}
#'  \item{actual_score}{Character represening the actual score in the fixture e.g. 2-1}
#'  \item{xg1}{Numeric fulltime home xG in the fixture}
#'  \item{xg2}{Numeric fulltime away xG in the fixture}
#'  \item{home}{Logical represening whether or not the team was home or away in the fixture}
#'  \item{xg_for}{Numeric showing running average of xG for the team}
#'  \item{xg_against}{umeric showing running average of xG against the team}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

get_xg_fixture_history <- function(home_scored_games_lag = 4,
                                   home_conceded_games_lag = 4,
                                   away_scored_games_lag = 4,
                                   away_conceded_games_lag = 4){

  # Create a vector and then loop through each to create a table
  xg_seasons_l <- list()

  # Consider downloading these as files to host these
  xg_season_url <- c("https://fbref.com/en/comps/9/1631/schedule/2017-2018-Premier-League-Scores-and-Fixtures",
                     "https://fbref.com/en/comps/9/1889/schedule/2018-2019-Premier-League-Scores-and-Fixtures",
                     "https://fbref.com/en/comps/9/3232/schedule/2019-2020-Premier-League-Scores-and-Fixtures",
                     "https://fbref.com/en/comps/9/10728/schedule/2020-2021-Premier-League-Scores-and-Fixtures",
                     "https://fbref.com/en/comps/9/schedule/Premier-League-Scores-and-Fixtures")

  for(i in 1:length(xg_season_url)){

    x <- read_html(xg_season_url[i])

    xg_seasons_l[i] <- html_nodes(x, "table") %>% html_table(fill = TRUE)

  }

  # As we are joining on fixture we need to tidy up the team names so they
  #   are consistent with what we see in the source history tables
  xg_seasons <- rbindlist(xg_seasons_l, use.names = TRUE) %>%
    select(date = .data$Date,
           home_team = .data$Home,
           away_team = .data$Away,
           actual_score = .data$Score,
           xg1 = 6,
           xg2 = 8) %>%
    mutate(home_team1 = case_when(
      home_team == "Leicester City" ~ "Leicester",
      home_team == "Manchester Utd" ~ "Man Utd",
      home_team == "Leeds United" ~ "Leeds",
      home_team == "Manchester City" ~ "Man City",
      home_team == "Swansea City" ~ "Swansea",
      home_team == "Cardiff City" ~ "Cardiff ",
      home_team == "Norwich City" ~ "Norwich",
      home_team == "Newcastle Utd" ~ "Newcastle",
      home_team == "Sheffield Utd" ~ "Sheffield United",
      home_team == "Stoke City" ~ "Stoke",
      home_team == "Tottenham" ~ "Spurs")) %>%
    mutate(home_team = ifelse(is.na(.data$home_team1), .data$home_team, .data$home_team1)) %>%
    mutate(away_team1 = case_when(
      away_team == "Leicester City" ~ "Leicester",
      away_team == "Manchester Utd" ~ "Man Utd",
      away_team == "Leeds United" ~ "Leeds",
      away_team == "Manchester City" ~ "Man City",
      away_team == "Swansea City" ~ "Swansea",
      away_team == "Cardiff City" ~ "Cardiff ",
      away_team == "Norwich City" ~ "Norwich",
      away_team == "Newcastle Utd" ~ "Newcastle",
      away_team == "Sheffield Utd" ~ "Sheffield United",
      away_team == "Stoke City" ~ "Stoke",
      away_team == "Tottenham" ~ "Spurs")) %>%
    mutate(away_team = ifelse(is.na(.data$away_team1), .data$away_team, .data$away_team1)) %>%
    mutate(fixture = paste0(.data$home_team,
                            " v ",
                            .data$away_team))

  xg_seasons_home <- xg_seasons %>%
    mutate(team = .data$home_team,
           home = TRUE) %>%
    select(.data$date,
           .data$team,
           .data$fixture,
           .data$actual_score,
           .data$xg1,
           .data$xg2,
           .data$home) %>% drop_na() %>%
    arrange(.data$team, .data$date) %>%
    distinct()


  xg_seasons_away <- xg_seasons %>%
    mutate(team = .data$away_team,
           home = FALSE) %>%
    select(.data$date,
           .data$team,
           .data$fixture,
           .data$actual_score,
           .data$xg1,
           .data$xg2,
           .data$home) %>% drop_na() %>%
    arrange(.data$team, .data$date) %>%
    distinct()

  xg_seasons_home_roll_xg <-
    # Home XG FOR and AGAINST
    cbind(xg_seasons_home,
          xg_for = xg_seasons_home  %>%
            pull(.data$xg1) %>%
            roll_mean(n = home_scored_games_lag, align = "right", fill = 0),
          xg_against = xg_seasons_home  %>%
            pull(.data$xg2) %>%
            roll_mean(n = home_conceded_games_lag, align = "right", fill = 0))

  xg_seasons_away_roll_xg <-
    # Away XG FOR and AGAINST
    cbind(xg_seasons_away,
          xg_for = xg_seasons_away  %>%
            pull(.data$xg2) %>%
            roll_mean(n = away_scored_games_lag, align = "right", fill = 0),
          xg_against = xg_seasons_away  %>%
            pull(.data$xg1) %>%
            roll_mean(n = away_conceded_games_lag, align = "right", fill = 0))

  xg_seasons_home_away <- rbind(xg_seasons_home_roll_xg,
                                xg_seasons_away_roll_xg) %>%
    mutate(date = ymd(.data$date))

  return(xg_seasons_home_away)

}
