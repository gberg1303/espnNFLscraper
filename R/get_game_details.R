get_game_details_helper <- function(espn_game_id){

  game_details <- espn_nfl_ids %>%
    dplyr::filter(espn_gameid == espn_game_id)

  message(
    paste("Pulling game details for", game_details$alt_gameid)
  )

  # Pull the JSon
  game_json <- httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(flatten = TRUE)

  # Add the Venue Information
  game_details <- game_details %>%
    dplyr::mutate(
      Stadium = game_json[["gameInfo"]][["venue"]][["fullName"]],
      Location = paste0(game_json[["gameInfo"]][["venue"]][["address"]][["city"]], ", ", game_json[["gameInfo"]][["venue"]][["address"]][["state"]]),
      Grass = game_json[["gameInfo"]][["venue"]][["grass"]]
    )

  # Add Officiating Crew
  if(is.null(game_json[["gameInfo"]][["officials"]]) == FALSE){
    officials <- game_json[["gameInfo"]][["officials"]] %>%
      dplyr::select(displayName, position.name) %>%
      tidyr::pivot_wider(names_from = position.name, values_from = displayName)

    # Merge officials back
    game_details <- cbind.data.frame(game_details, officials)
  } else {
    message(
      paste("Officials unavailable for", game_details$alt_gameid)
    )
  }

  return(game_details)
}


#' Get Game Details
#'
#' Pull game location, turf type, and officiating crew from multiple games
#' @param espn_gameids: The game IDs from ESPN. These can be pulled from the espn_nfl_ids dataset included with the package.
#' @return Data with game location, turf type, and officiating crew from multiple games
#' @examples
#' get_game_details(espn_gameids = espn_nfl_ids %>% filter(season == 2019 & season_type == 2) %>% pull(espn_gameid))
#' @export
get_game_details <- function(espn_gameids){
  purrr::map_df(espn_gameids, function(x){get_game_details_helper(x)})
}
