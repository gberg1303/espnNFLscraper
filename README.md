# espnNFLscraper
Get ESPN NFL Data via Their API

## How to install
```
remotes::install_github("gberg1303/espnNFLscraper")
```

## Advice on Use
Start with the espn_nfl_ids dataset. This has basic information about games played from 2002 to now. When using functions that rely on a list of game ids from ESPN, it may be useful to filter the espn_nfl_ids dataset by season, season type (regular, postseason, etc) and pull the list ids given your respective criteria
