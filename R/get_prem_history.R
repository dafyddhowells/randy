#' Get all historic Premier League results
#'
#' Extract current season Premier League match data and combines it with previous seasons in a data frame.
#'
#' @import dplyr
#' @import lubridate
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom tidyr separate
#' @importFrom tidyr replace_na
#' @importFrom tidyr drop_na
#' @export
#' @examples
#' \dontrun{
#' prem_history <- get_prem_history()
#' }
#' @return This function returns a \code{data.frame} with the following columns:
#' \describe{
#'  \item{div}{Character representing the division, default E0 which is the value for the Premier League}
#'  \item{date}{Date when the fixture was played}
#'  \item{home_team}{Character showing team playing at home}
#'  \item{home_team}{Character showing team playing away}
#'  \item{fthg}{Numeric fulltime home goals scored in the fixture}
#'  \item{ftag}{Numeric fulltime away goals scored in the fixture}
#' }
#' @author Daf Howells <dafhowells@yahoo.co.uk>

get_prem_history <- function(){

#load("~/Documents/randy/data/prem_2018.rda")
#load("~/Documents/randy/data/prem_2019.rda")
#load("~/Documents/randy/data/prem_2020.rda")
#load("~/Documents/randy/data/prem_pre_2018.rda")

  # Pull in fixtures and teams from FPL API
  ff_bootstrap <- fromJSON("https://fantasy.premierleague.com/api/bootstrap-static/")

  # Get teams
  ff_teams <- tibble(ff_bootstrap[["teams"]]) %>%
    select(.data$id,
           .data$name) %>%
    mutate(id = as.character(.data$id))

  # Get fixtures and tidy up team names home and away
  ff_fixtures <- tibble(fromJSON("https://fantasy.premierleague.com/api/fixtures/")) %>%
    select(game_week = .data$event,
           fixture_no = .data$id,
           .data$finished,
           .data$kickoff_time,
           home = .data$team_h,
           home_score = .data$team_h_score,
           away = .data$team_a,
           away_score = .data$team_a_score) %>%
    mutate(home = as.character(.data$home),
           away = as.character(.data$away)) %>%
    mutate(home = case_when(home == "1" ~ "Arsenal",
                            home == "2" ~ "Aston Villa",
                            home == "3" ~ "Brentford",
                            home == "4" ~ "Brighton",
                            home == "5" ~ "Burnley",
                            home == "6" ~ "Chelsea",
                            home == "7" ~ "Crystal Palace",
                            home == "8" ~ "Everton",
                            home == "9" ~ "Leicester",
                            home == "10" ~ "Leeds",
                            home == "11" ~ "Liverpool",
                            home == "12" ~ "Man City",
                            home == "13" ~ "Man Utd",
                            home == "14" ~ "Newcastle",
                            home == "15" ~ "Norwich",
                            home == "16" ~ "Southampton",
                            home == "17" ~ "Spurs",
                            home == "18" ~ "Watford",
                            home == "19" ~ "West Ham",
                            home == "20" ~ "Wolves")) %>%
    mutate(away = case_when(away == "1" ~ "Arsenal",
                            away == "2" ~ "Aston Villa",
                            away == "3" ~ "Brentford",
                            away == "4" ~ "Brighton",
                            away == "5" ~ "Burnley",
                            away == "6" ~ "Chelsea",
                            away == "7" ~ "Crystal Palace",
                            away == "8" ~ "Everton",
                            away == "9" ~ "Leicester",
                            away == "10" ~ "Leeds",
                            away == "11" ~ "Liverpool",
                            away == "12" ~ "Man City",
                            away == "13" ~ "Man Utd",
                            away == "14" ~ "Newcastle",
                            away == "15" ~ "Norwich",
                            away == "16" ~ "Southampton",
                            away == "17" ~ "Spurs",
                            away == "18" ~ "Watford",
                            away == "19" ~ "West Ham",
                            away == "20" ~ "Wolves")) %>%
    mutate(result = case_when(.data$home_score == .data$away_score ~ "Draw",
                              .data$home_score > .data$away_score ~ "Home Win",
                              .data$home_score < .data$away_score ~ "Away Win")) %>%
    mutate(score = paste(.data$home_score, .data$away_score, sep = "-"))


  ff_results <- ff_fixtures %>%
    mutate(fixture = paste(home, away, sep = " v ")) %>%
    select(.data$finished,
           .data$fixture_no,
           .data$fixture,
           actual_result = .data$result,
           actual_score = .data$score)

  # Prem results from this season
  prem_2021 <- ff_results %>%
    select(.data$fixture_no,
           .data$fixture,
           .data$actual_score) %>%
    separate(.data$actual_score,
                    sep = "-",
                    c("fthg", "ftag")) %>%
    separate(.data$fixture,
                    sep = " v ",
                    c("home_team", "away_team")) %>%
    inner_join(ff_fixtures %>% select(.data$fixture_no, .data$kickoff_time),
               by = "fixture_no") %>%
    mutate(date_ext = substr(kickoff_time, 1, 10)) %>%
    mutate(date = paste(substr(date_ext, 9, 10),
                        substr(date_ext, 6, 7),
                        substr(date_ext, 1, 4),
                        sep = "/"),
           div = "E0") %>%
    select(.data$div, .data$date, .data$home_team, .data$away_team, .data$fthg, .data$ftag)

  # Bind all previous history tables with current season
  prem_history <- rbind(prem_pre_2018, prem_2020, prem_2019, prem_2018, prem_2021) %>%
    drop_na() %>%
    filter(!fthg == "NA") %>%
    mutate(fthg = as.numeric(fthg),
           ftag = as.numeric(ftag)) %>%
    mutate(home_team = ifelse(home_team == "Man United", "Man Utd", home_team)) %>%
    mutate(home_team = ifelse(home_team == "Tottenham", "Spurs", home_team)) %>%
    mutate(away_team = ifelse(away_team == "Man United", "Man Utd", away_team)) %>%
    mutate(away_team = ifelse(away_team == "Tottenham", "Spurs", away_team)) %>%
    mutate(date = dmy(date)) %>%
    mutate(fixture = paste0(home_team, " v ", away_team)) %>%
    select(-div)

  return(prem_history)

}
