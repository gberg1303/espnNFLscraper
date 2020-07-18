get_espn_wp_helper <- function(espn_game_id) {
  espn_wp <- data.frame()
  tryCatch(
    expr = {
      espn_pbp <-
        httr::GET(url = glue::glue("http://site.api.espn.com/apis/site/v2/sports/football/nfl/summary?event={espn_game_id}")) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(flatten = TRUE)

      espn_plays <-
        purrr::map_df(espn_pbp$drives$previous$plays, ~.x) %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          espn_game_id = stringr::str_sub(id, end = stringr::str_length(espn_game_id)),
          play_id = stringr::str_sub(id, stringr::str_length(espn_game_id) + 1),
          qtr = period_number,
          time = clock_display_value,
          play_description = text,
          quarter_seconds_remaining = lubridate::period_to_seconds(lubridate::ms(time)),
          half_seconds_remaining = dplyr::if_else(
            qtr %in% c(1, 3),
            quarter_seconds_remaining + 900,
            quarter_seconds_remaining
          ),
          reg_game_seconds_remaining = dplyr::if_else(
            qtr <= 4,
            quarter_seconds_remaining + (900 * (4 - as.numeric(qtr))),
            NA_real_
          ),
          all_game_seconds_remaining = dplyr::if_else(
            qtr <= 4,
            reg_game_seconds_remaining +
              dplyr::if_else(
                max(qtr) > 4,
                dplyr::first(quarter_seconds_remaining[qtr > 4]) -
                  dplyr::nth(quarter_seconds_remaining[qtr > 4], -2),
                0
              ),
            quarter_seconds_remaining - dplyr::nth(quarter_seconds_remaining, -2)
          )
        ) %>%
        select(espn_game_id:all_game_seconds_remaining)

      espn_wp <- espn_pbp$winprobability %>%
        janitor::clean_names() %>%
        dplyr::mutate(
          play_id = stringr::str_sub(play_id, stringr::str_length(espn_game_id) + 1),
          home_wp_pre_play = dplyr::lag(home_win_percentage, 1),
          home_wp_post_play = home_win_percentage
        ) %>%
        dplyr::left_join(espn_plays, by = "play_id") %>%
        dplyr::select(espn_game_id, play_id:all_game_seconds_remaining) %>%
        dplyr::slice(-1) %>%
        dplyr::mutate(espn_game_id = as.integer(espn_game_id)) %>%
        dplyr::right_join(y = espn_nfl_ids %>% dplyr::filter(espn_gameid == espn_game_id), x = ., by = c("espn_game_id" = "espn_gameid"))

      message(glue::glue("{Sys.time()}: Scraping ESPN wp data for GameID '{espn_game_id}'..."))
    },
    error = function(e) {
      message(glue::glue("{Sys.time()}: GameID '{espn_game_id}' failed!"))
    },
    warning = function(w) {
    },
    finally = {
      # Sys.sleep(0.1)
      return(espn_wp)
    }
  )
}

#' Get WP from NFL Game
#'
#' Pull ESPN's WP at each point in the game. Function created by github.com/mrcaseb
#' @param espn_gameids the gameids from espn
#' @return dataframe with basic espn's WP for each team after plays in the game
#' @examples
#' # Pull the espn_gameid already available in the espn_nfl_ids ID dataset
#' get_game_wp(espn_gameids = espn_nfl_ids %>% filter(season == 2019 & season_type == 2 & week == 1) %>% pull(espn_gameid))
#' @export
get_game_wp <- function(espn_gameids){
  purrr::map_df(espn_gameids, function(x){get_espn_wp_helper(x)})
}
