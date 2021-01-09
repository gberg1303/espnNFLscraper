#' Get Game IDs
#'
#' Pull ESPN's GameIDs for NFL Games
#' @import dplyr
#' @param season: the specific season for which you want to grab gameids from ESPN
#' @param season_type: "preseason", "regular", or "postseason".
#' @return dataframe with basic game info and espn gameids
#' @examples
#' get_game_ids(season = 2019, season_type = "regular")
#' @export
get_game_ids <- function(season, season_type = c("preseason", "regular", "postseason")){
  current_year <- as.double(substr(Sys.Date(), 1, 4))
  espn_game_ids <- data.frame()

  if (!season_type %in% c("preseason", "regular", "postseason", "all")) {
    stop("Please choose season_type of 'regular',  'playoffs', 'postseason', or 'all'")
  }

  if (!dplyr::between(as.numeric(season), 2002, current_year)) {
    stop(paste("Please choose season between 2002 and", current_year))
  }

  if(lubridate::month(Sys.Date()) < 12 & lubridate::month(Sys.Date()) > 2 & season_type == "postseason" & current_year == season | season_type == "postseason" & lubridate::month(Sys.Date()) <= 2 & current_year == season){
    stop(paste("Unfortunately, the NFL Playoff Games have not been determined yet"))
  }

  message(
    dplyr::if_else(
      season_type == "regular",
      glue::glue("Scraping from {season} {season_type} season!"),
      glue::glue("Scraping from {season} {season_type}!")
    )
  )

  season_type <- ifelse(season_type == "preseason", "1", season_type)
  season_type <- ifelse(season_type == "regular", "2", season_type)
  season_type <- ifelse(season_type == "postseason", "3", season_type)

  weeks <- ifelse(season_type == "2", 17, 5)

   espn_game_ids <- purrr::map_df(1:weeks, function(week){
      url <- glue::glue("https://www.espn.com/nfl/schedule/_/week/{week}/year/{season}/seasontype/{season_type}")

      webpage <- xml2::read_html(url)

      links <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href")

      espn_gameid <- links %>%
        tibble::as.tibble() %>%
        dplyr::filter(str_detect(value, "gameId") == TRUE) %>%
        dplyr::pull(value) %>%
        stringr::str_remove(., "/nfl/game/_/gameId/")

      bye_teams <- webpage %>%
        rvest::html_nodes(".odd.byeweek" ) %>%
        rvest::html_nodes("abbr") %>%
        rvest::html_text()

      home_team <- webpage %>%
        rvest::html_nodes(".home-wrapper") %>%
        rvest::html_nodes("abbr") %>%
        rvest::html_text()

      away_team <- webpage %>%
        rvest::html_nodes("abbr") %>%
        rvest::html_text()
      away_team <- away_team[!away_team %in% home_team]
      away_team <- away_team[!away_team %in% bye_teams]

      placeholder <- data.frame(
        home_team,
        away_team,
        espn_gameid
      ) %>%
        dplyr::mutate(
          season_type = season_type,
          season = season,
          week = ifelse(season_type == 3, 17 + week, week)
        )

      espn_game_ids <- dplyr::bind_rows(espn_game_ids, placeholder)
      return(espn_game_ids)

    }
      )

  ### Fix Several Names for Compatibility with nflfastR Data game_ids
  espn_game_ids <- espn_game_ids %>%
    dplyr::mutate(
      home_team = gsub("WSH", "WAS", home_team),
      away_team = gsub("WSH", "WAS", away_team),
      home_team = gsub("LAR", "LA", home_team),
      away_team = gsub("LAR", "LA", away_team)
    ) %>%
  # Add nflfastR game_ids
    dplyr::mutate(
      week = ifelse(week == 22, week-1, week),
      alt_gameid = paste0(season, "_", ifelse(week >= 10, paste0(week), paste0(0,week)), "_", away_team, "_", home_team)
    )

  return(espn_game_ids)
}
