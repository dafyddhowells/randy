join_form_and_fixture_history <- function() {

  get_prem_form() %>%
    left_join(
      get_prem_fixture_history(),
      by = c("fixture" = "fixture",
             "date" = "date",
             "team" = "team",
             "home" = "home")) %>%
    dplyr::select(-fthg.y, -ftag.y) %>%
    rename(fthg = fthg.x, ftag = ftag.x)

}
