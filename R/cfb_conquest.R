#' cfb_conquest
#' @description Provides a data frame with current college football conquest data during a particular season and week. The data frame is ready for plotting with hf_map().
#'
#' @param season (Integer required): Year, 4 digit format (YYYY), ~1897 to present.
#' @param week (Integer required): Week, values from 1-15, 1-14 for seasons pre-playoff (i.e. 2013 or eariler).
#' @param division (String required): What is the lowest division that should start off owning land? Acceptable inputs include "fbs" or "fcs". \emph{Defaults to "fbs"}.
#'
#' @importFrom rlang .data
#' @return Returns data frame prepared for hf_map().
#' @export
#'
#' @examples
#' \dontrun{cfb_conquest(season = 2016, week = 8)}
cfb_conquest <- function(season, week, division = "fbs"){

  alt_color_list <- get0("alt_color_list", envir = asNamespace("territorymap"))
  alt_logo_list <- get0("alt_logo_list", envir = asNamespace("territorymap"))

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

  # checking for valid division
  if(!(division %in% c("fbs","fcs","ii","iii"))){
    stop("Division must be string with specifying either fbs, fcs, ii, or iii")
  }

  # determining land owners from input
  if (division == "fbs") {
    land_owners <- c("fbs")
  } else if (division == "fcs") {
    land_owners <- c("fbs", "fcs")
  }

  clean_teams <- function(teams){

    # getting best colors
    teams <- teams |>
      dplyr::mutate(best_color = dplyr::case_when(
        .data$school %in% alt_color_list ~ alt_color,
        !.data$school %in% alt_color_list ~ color
      ))

    # getting best logos
    teams <- teams |>
      dplyr::mutate(best_logo = dplyr::case_when(
        .data$school %in% alt_logo_list ~ logo_2,
        !.data$school %in% alt_logo_list ~ logo
      ))

    return(teams)
  }

  if(week == 0){

    teams <- cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season)|>
      dplyr::filter(.data$classification %in% land_owners) |>
      clean_teams() |>
      dplyr::select(.data$school,
                    .data$latitude,
                    .data$longitude,
                    .data$best_color,
                    .data$best_logo) |>
      dplyr::rename(entity = .data$school,
                    lat = .data$latitude,
                    lng = .data$longitude,
                    color = .data$best_color,
                    image = .data$best_logo)

  } else{

    for(week_number in 1:week){

      if(week_number == 1){
        game_info <- cfbfastR::cfbd_game_info(year = season,
                                              week = week_number)

      } else{
        game_info <- rbind(game_info,
                           cfbfastR::cfbd_game_info(year = season,
                                                    week = week_number))
      }
    }

    game_info_select <- game_info |>
      dplyr::mutate(
        winner = dplyr::if_else(.data$home_points > .data$away_points,
                                .data$home_id,
                                .data$away_id),
        loser = dplyr::if_else(.data$home_points < .data$away_points,
                               .data$home_id,
                               .data$away_id)
      ) |>
      dplyr::select(.data$winner,
                    .data$loser)

    teams <- cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season)|>
      dplyr::filter(.data$classification %in% land_owners) |>
      dplyr::select(.data$team_id,
                    .data$latitude,
                    .data$longitude)

    for (i in 1:nrow(game_info_select)) {
      # Get the winner and loser team IDs
      winner_id <- game_info_select$winner[i]
      loser_id <- game_info_select$loser[i]

      # Find the row in teams with the loser team ID and replace it with the winner team ID
      teams$team_id[teams$team_id == loser_id] <- winner_id
    }

    all_teams <- cfbfastR::cfbd_team_info(only_fbs = FALSE, year = season) |>
      dplyr::select(.data$team_id,
                    .data$school,
                    .data$color,
                    .data$alt_color,
                    .data$logo,
                    .data$logo_2) |>
      clean_teams()

    all_teams <- all_teams |>
      dplyr::select(.data$team_id,
                    .data$school,
                    .data$best_color,
                    .data$best_logo)

    teams <- dplyr::left_join(teams,
                              all_teams,
                              by = "team_id") |>
      dplyr::rename(entity = .data$school,
                    color = .data$best_color,
                    image = .data$best_logo,
                    lat = .data$latitude,
                    lng = .data$longitude) |>
      # selecting everything except for team_id
      dplyr::select(.data$entity,
                    .data$lat,
                    .data$lng,
                    .data$color,
                    .data$image) |>
      dplyr::arrange(.data$entity)

  }

  # checking for missing colors
  if (any(is.na(teams$color))) {
    # Fill in missing values with white
    teams$color[is.na(teams$color)] <- "#FFFFFF"
  }

  # checking for missing logos
  if (any(is.na(teams$image))) {
    # Fill in missing values with NCAA football logo
    teams$image[is.na(teams$image)] <- "./inst/figures/no_logo.png"
  }

  return(teams)

}

