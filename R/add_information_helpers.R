#' Add Gamescores
#'
#' Add the outcomes of completed NFL games and discover if the home team won.
#' @param espnNFLscraper_dataset : a dataset created by espnNFLscraper
#' @return dataframe with the outcomes of completed NFL games.
#' @examples
#' get_game_details(espn_gameids = espn_nfl_ids %>% filter(season == 2019 & season_type == 2) %>% pull(espn_gameid)) %>% add_gamescores()
#' @export
add_gamescores <- function(espnNFLscraper_dataset){

  if(is.null(espnNFLscraper_dataset$season == TRUE))
    stop("Please ensure that the dataset has a proper 'season' column")

  seasons <- min(espnNFLscraper_dataset$season):max(espnNFLscraper_dataset$season)

  ### Get Weekly Data for Merging
  Fast_R_Merger <- nflfastR::fast_scraper_schedules(seasons) %>%
    dplyr::select(game_id, home_score, away_score) %>%
    dplyr::mutate(home_win = ifelse(home_score > away_score, 1, 0))

  ### Merge with the file
  espnNFLscraper_dataset <- espnNFLscraper_dataset %>%
    left_join(Fast_R_Merger, by = c("alt_gameid" = "game_id"))

  return(espnNFLscraper_dataset)

}


#' Add FiveThirtyEight's Elo Predictions
#'
#' Add the pregame predictions of FiveThirtyEight's Elo projection system
#' @param espnNFLscraper_dataset : a dataset created by espnNFLscraper
#' @return dataframe with 538's predictions appended.
#' @examples
#' get_game_details(espn_gameids = espn_nfl_ids %>% filter(season == 2019 & season_type == 2) %>% pull(espn_gameid)) %>% add_fivethirtyeight_predictions()
#' @export
add_fivethirtyeight_predictions <- function(espnNFLscraper_dataset){

  if(is.null(espnNFLscraper_dataset$season == TRUE))
    stop("Please ensure that the dataset has a proper 'season' column")

  ### Add Date to Dataset for merging purposes
  seasons <- min(espnNFLscraper_dataset$season):max(espnNFLscraper_dataset$season)
  Fast_R_Merger <- nflfastR::fast_scraper_schedules(seasons) %>%
    dplyr::select(game_id, gameday, home_team, away_team) %>%
    dplyr::mutate(
      gameday = lubridate::as_date(gameday)
    )

  ### Load Elo Data
  FiveThirtyEight_Elo <- read_csv("https://projects.fivethirtyeight.com/nfl-api/nfl_elo.csv") %>%
    dplyr::mutate(team1 = gsub("WSH", "WAS", team1),
           team2 = gsub("WSH", "WAS", team2),
           team1 = gsub("LAR", "LA", team1),
           team2 = gsub("LAR", "LA", team2)) %>%
    dplyr::select(date, season, team1, team2, elo_prob1) %>%
    dplyr::rename(fivethirtyeight_home_wp = elo_prob1)

  ### Merge FiveThirtyEight with Weekly Data
  espnNFLscraper_dataset <- espnNFLscraper_dataset %>%
    left_join(Fast_R_Merger, by = c("alt_gameid" = "game_id", "home_team", "away_team")) %>%
    left_join(FiveThirtyEight_Elo, by = c( "gameday" = "date", "season", "home_team" = "team1", "away_team" = "team2"))

  return(espnNFLscraper_dataset)

  }
