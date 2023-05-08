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

  # gathering desired logo and color lists from sysdata.rda
  alt_color_list <- get0("alt_color_list", envir = asNamespace("homefield"))
  alt_logo_list <- get0("alt_logo_list", envir = asNamespace("homefield"))

  # converting to numeric if inputted as string
  season <- as.numeric(season)
  week <- as.numeric(week)

  # Defining a function to retrieve game information for a specific set of weeks
  get_game_info <- function(week_number, season) {
    cfbfastR::cfbd_game_info(year = season, week = week_number)
  }

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

  # Querying CFB Database -----------------------------------------------------

  # if week 0, return all teams since all are undefeated
  if(week == 0){

    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

  } else{
  # if not week 0, determine which teams are undefeated
  # by looking at game result data

    # Use lapply() to retrieve game information for all desired weeks
    game_info_list <- lapply(1:week, get_game_info, season)

    # Use do.call() to combine the list of data frames into a single data frame
    game_info <- do.call(rbind, game_info_list)

    # determining which teams have lost
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

    # getting team info such as location, color, and logo
    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

    # removing teams that have been defeated
    teams <- teams[!(teams$team_id %in% losers$loser), ]
  }

  # Cleaning Dataframe -----------------------------------------------------

  # determining best colors
  teams <- teams |>
    dplyr::mutate(best_color = dplyr::case_when(
      school %in% alt_color_list ~ alt_color,
      !school %in% alt_color_list ~ color
    ))

  # determining best logos
  teams <- teams |>
    dplyr::mutate(best_logo = dplyr::case_when(
      school %in% alt_logo_list ~ logo_2,
      !school %in% alt_logo_list ~ logo
    ))

  # cleaning up dataframe
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
                  image = best_logo) |>
    as.data.frame()

  # checking for missing colors
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




