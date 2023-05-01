#' cfb_undefeated
#' @description Provides a data frame with the undefeated D1 College Football teams during a particular season and week. The data frame is ready for plotting with hf_map().
#'
#' @param season (Integer required): Year, 4 digit format (YYYY), ~1897 to present.
#' @param week (Integer required): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or eariler).
#'
#' @importFrom rlang .data
#' @return Returns data frame prepared for homefield_map().
#' @export
#'
#' @examples
#' \dontrun{cfb_undefeated(season = 2016, week = 8)}
cfb_undefeated <- function(season, week){

  # passes cmd check
  home_points <- home_id <- away_id <- away_points <- loser <- school <- NULL
  latitude <- longitude <- best_color <- best_logo <- NULL

  alt_color_list <- get0("alt_color_list", envir = asNamespace("homefield"))
  alt_logo_list <- get0("alt_logo_list", envir = asNamespace("homefield"))

  # converting to numeric if inputted as string
  season <- as.numeric(season)
  week <- as.numeric(week)

  # Input Checks --------------------------------------------------------------

  # check for valid season
  if(season > as.numeric(format(Sys.Date(), "%Y")) | season < 1897){
    stop("Season must be the current season or any past season after ~1897.")
  }

  # check for valid week
  if(!season %% 1 == 0){
    stop("Season must be an integer.")
  }

  if(!week %% 1 == 0 | week > 15 | week < 0 | (week > 14 & season < 2014)){
    stop("Week must be an integer with values between 1-15 or 1-14 for seasons prior to 2013.")
  }

  if(week == 0){

    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

  } else{

    for(week_number in 1:week){

      if(week_number == 1){
        game_info <- cfbfastR::cfbd_game_info(year = season, week = week_number)

      } else{
        game_info <- rbind(game_info, cfbfastR::cfbd_game_info(year = season,
                                                               week = week_number))
      }
    }

    losers <- game_info |>
      dplyr::mutate(
        winner = dplyr::if_else(home_points > away_points,
                                home_id,
                                away_id),
        loser = dplyr::if_else(home_points < away_points,
                               home_id,
                               away_id)
      ) |>
      dplyr::select(loser) |>
      unique()

    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

    teams <- teams[!(teams$team_id %in% losers$loser), ]
  }


  # getting best colors
  teams <- teams |>
    dplyr::mutate(best_color = dplyr::case_when(
      school %in% alt_color_list ~ alt_color,
      !school %in% alt_color_list ~ color
    ))

  # getting best logos
  teams <- teams |>
    dplyr::mutate(best_logo = dplyr::case_when(
      school %in% alt_logo_list ~ logo_2,
      !school %in% alt_logo_list ~ logo
    ))

  teams <- teams |>
    dplyr::arrange(school) |>
    dplyr::select(school,
                  latitude,
                  longitude,
                  best_color,
                  best_logo) |>
    dplyr::rename(entity = school,
                  lat = latitude,
                  lng = longitude,
                  color = best_color,
                  image = best_logo)

  if (any(is.na(teams$color))) {
    # Fill in missing values with white
    teams$color[is.na(teams$color)] <- "#FFFFFF"
  }

  # checking for missing logos
  if (any(is.na(teams$image))) {
    # Fill in missing values with NCAA football logo
    teams$image[is.na(teams$image)] <- "https://github.com/Landon-Getting/homefield-graphics/blob/main/no_logo.png?raw=true"
  }

  return(teams)

}




