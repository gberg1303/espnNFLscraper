% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_game_details.R
\name{get_game_details}
\alias{get_game_details}
\title{Get Game Details}
\usage{
get_game_details(espn_gameids)
}
\arguments{
\item{espn_gameids:}{The game IDs from ESPN. These can be pulled from the espn_nfl_ids dataset included with the package.}
}
\value{
Data with game location, turf type, and officiating crew from multiple games
}
\description{
Pull game location, turf type, and officiating crew from multiple games
}
\examples{
get_game_details(espn_gameids = espn_nfl_ids \%>\% filter(season == 2019 & season_type == 2) \%>\% pull(espn_gameid))
}
