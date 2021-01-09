#' Gets a single game's predictions from ESPN. Mainly used as a helper for get_pregame_predictions
prediction_helper <- function(espn_game_id){

  pregame_predictions <- data.frame()

  # Pull the JSon
  game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)


  # Pull the game data from the ID dataframe
  pregame_predictions <- readRDS(url("http://www.habitatring.com/games_alt.rds")) %>%
    dplyr::filter(espn == espn_game_id) %>%
    dplyr::select(home_team, away_team, espn, season, week, game_id) %>%
    dplyr::mutate(season_type = ifelse(week >= 18, 3, 2)) %>%
    dplyr::rename(alt_gameid = game_id,
                  espn_gameid = espn)
  if("gameProjection" %in% names(game_json[["predictor"]][["homeTeam"]]) == TRUE){
  pregame_predictions <- pregame_predictions %>%
    dplyr::mutate(
      espn_home_wp = as.numeric(game_json[["predictor"]][["homeTeam"]][["gameProjection"]])/100
    )
    message(
      paste("Pulling predictions for", pregame_predictions$alt_gameid)
    )
  }


  # Grab and convert the Moneylines from Oddsmakers
  if("pickcenter" %in% names(game_json) == TRUE &
     "provider.name" %in% names(game_json[["pickcenter"]]) == TRUE &
     "homeTeamOdds.moneyLine" %in% names(game_json[["pickcenter"]]) == TRUE
     ){
  vegas_odds <- data.frame(
    providers = game_json[["pickcenter"]][["provider.name"]],
    odds = ifelse(game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] > 0, 100/(game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]] + 100), game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]]/(game_json[["pickcenter"]][["homeTeamOdds.moneyLine"]]-100))
    ) %>%
    tidyr::pivot_wider(names_from = providers, values_from = odds)

  pregame_predictions <- cbind(
    pregame_predictions, vegas_odds
  )

  }

  return(pregame_predictions)
}


#' Get Pregame Predictions
#'
#' Pull and combine pregame predictions from multiple games
#' @param espn_gameids: The game IDs from ESPN. These can be pulled from the espn_nfl_ids dataset included with the package.
#' @return Data with basic game info and pregame home team win probabilities
#' @examples
#' # Pull the espn_gameid already available in the espn_nfl_ids ID dataset
#' get_pregame_predictions(espn_nfl_ids %>% filter(season == 2019 & season_type == 2) %>% pull(espn_gameid))
#' @export
get_pregame_predictions <- function(espn_gameids){
  purrr::map_df(espn_gameids, function(x){prediction_helper(x)})
}




