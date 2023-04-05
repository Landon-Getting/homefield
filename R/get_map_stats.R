#' get_map_stats
#' @description Returns a data frame with summary statistics for each identifier based on their map territory. \cr
#' Note that this function requires a CENSUS API Key.
#' Follow the directions \href{https://walker-data.com/tidycensus/reference/census_api_key.html}{here}
#' to receive a key. \cr
#' \cr
#' Output data frame includes four summary statistics:\cr
#' \cr
#' \strong{land} - Total land area area (in square meters) within territory controlled by identifier.\cr
#' \cr
#' \strong{water} - Total land area area (in square meters) within territory controlled by identifier.\cr
#' \cr
#' \strong{domain} - Sum of land and water area (in square meters) within territory controlled by identifier.\cr
#' \cr
#' \strong{pop} - Population that resides within territory controlled by identifier.\cr
#'
#'
#' @param x Data frame created by other territorymap functions or including the following columns:\cr
#' \cr
#' \strong{identifier} - identifies each element (ex. school name - Iowa State, Minnesota, Bowling Green).\cr
#' \cr
#' \strong{lat} - latitude of element.\cr
#' \cr
#' \strong{lng} - longitude of element.\cr
#'
#' Columns must also be of equal length and match the specified names exactly.
#'
#' @param continental (Boolean required): If TRUE (default), only considers the continental United States.
#' If FALSE, also considers Alaska and Hawaii.
#'
#' @param temporal (Vector required): If argument declared with a vector, x must be a list of data frames.
#' The length of the vector supplied must equal the number of data frames.
#' The vector should contain temporal values representing the respective time of each data frame.
#' This will alter the returned data frame to include an extra column named 'time' with the supplied temporal information in a tidy format.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # standard example
#' cfb_map_stats <- get_map_stats(get_cfb_undefeated(season = 2021, week = 6))
#'
#' # temporal example
#'  x_input <- list(get_cfb_undefeated(season = 2021, week = 0),
#'                  get_cfb_undefeated(season = 2021, week = 1),
#'                  get_cfb_undefeated(season = 2021, week = 2),
#'                  get_cfb_undefeated(season = 2021, week = 3),
#'                  get_cfb_undefeated(season = 2021, week = 4))
#'
#'  # week 0 through 4
#' temporal_input <- c(0,
#'                      1,
#'                      2,
#'                      3,
#'                      4)
#'
#'  temporal_stats <- territorymap::get_map_stats(x = x_input, temporal = temporal_input)
#'}
get_map_stats <- function(x, continental = TRUE, temporal){

  if(!is.logical(continental)){
    stop("Continental must be a boolean value.")
  }

  if(continental == TRUE){
    STUSPS_filter <- c("VI", "PR", "GU", "AS", "MP", "UM", "AK", "HI")
  } else{
    STUSPS_filter <- c("VI", "PR", "GU", "AS", "MP", "UM")
  }

  # input data checks ----------------------------------------------------------
  input_checks <- function(x){
    # setting expected column names
    expected_cols <- c("identifier",
                       "lat",
                       "lng")

    # checking if column names match expected names
    if (!all(expected_cols %in% names(x))) {
      stop("x must contain columns identifier, lat, and lng.")
    }

    # getting column sizes
    col_lengths <- sapply(x, length)

    # Checking if all column lengths are equal
    if (!length(unique(col_lengths)) == 1) {
      stop("x must contain columns of equal size.")
    }

    # Defining a regular expression for matching valid latitude values
    lat_regex <- "^[-+]?([1-8]?\\d(\\.\\d+)?|90(\\.0+)?)$"

    # Checking if the latitude column contains valid latitude values
    if (!all(grepl(lat_regex, x$lat, ignore.case = TRUE))) {
      stop("x must contain a lat column with valid latitudes.")
    }

    # Defining a regular expression for matching valid longitude values
    long_regex <- "^[-+]?((1[0-7]|[1-9])?\\d(\\.\\d+)?|180(\\.0+)?)$"

    # Checking if the longitude column contains valid longitude values
    if (!all(grepl(long_regex, x$lng, ignore.case = TRUE))) {
      stop("x must contain a lng column with valid longitudes")
    }
  }

  generate_stats <- function(x){

    input_df <- x

    input_df_location <- input_df |>
      dplyr::select(.data$identifier,
                    .data$lat,
                    .data$lng)

    # converting lat/long to sf point object -------------------------------------
    input_df_location <-  sf::st_as_sf(input_df_location,
                                       coords = c("lng", "lat"),
                                       crs = 4326) |>
      dplyr::rename(location = .data$geometry)

    counties <- tigris::counties(cb = TRUE, progress_bar = FALSE) |>
      dplyr::filter(!.data$STUSPS %in% STUSPS_filter) |> # filtering out territories
      sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
      suppressMessages()

    counties$centroid <- sf::st_centroid(counties$geometry)

    # Calculating Distance between every county and identifier -----------------------
    comparing_distances <- tidyr::expand_grid(input_df_location,
                                              counties |>
                                                dplyr::select(.data$GEOID, .data$centroid) |>
                                                sf::st_drop_geometry())

    comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
    comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

    comparing_distances$distances <- sf::st_distance(comparing_distances$location$x,
                                                     comparing_distances$centroid$x,
                                                     by_element = TRUE)

    comparing_distances <- comparing_distances |>
      dplyr::select(.data$GEOID,
                    .data$identifier,
                    .data$distances) |>
      dplyr::group_by(.data$GEOID) |>
      dplyr::slice(which.min(.data$distances))

    # Create data frame to match each county to a school  ------------------------
    territory_map_df <- comparing_distances |>
      dplyr::select(.data$GEOID,
                    .data$identifier) |>
      dplyr::left_join(counties |>
                         dplyr::select(.data$GEOID,
                                       .data$ALAND,
                                       .data$AWATER),
                       by = "GEOID",
                       keep = FALSE)

    territory_map_df <- sf::st_as_sf(territory_map_df)
    territory_map_df <- sf::st_drop_geometry(territory_map_df)

    # Getting population data  ---------------------------------------------------
    counties_pop <- tidycensus::get_estimates(geography = "county",
                                              product = "population") |>
      dplyr::filter(.data$variable != "DENSITY") |>
      dplyr::select(.data$GEOID,
                    .data$value) |>
      dplyr::rename(population = .data$value)

    territory_map_df <- dplyr::left_join(territory_map_df,
                                         counties_pop,
                                         by = "GEOID")

    # Calculating map stats  ---------------------------------------------------
    return(
      territory_map_df |>
      dplyr::group_by(.data$identifier) |>
      dplyr::summarise(land = sum(ALAND, na.rm = TRUE),
                       water = sum(AWATER, na.rm = TRUE),
                       domain = sum(ALAND, na.rm = TRUE) + sum(AWATER, na.rm = TRUE),
                       pop = sum(population, na.rm = TRUE))
      )
  }

  # temporal? ----------------------------------------------------------------

  if(!missing(temporal)){ # temporal argument provided

    # check to ensure length of temporal matches length of x
    if(length(temporal) != length(x)){
      stop("The length of temporal must match the number of data frames in x.")
    }

    # check if temporal is a vector
    if(!is.vector(temporal)){
      stop("temporal must be a vector.")
    }

    # performing checks on each data frame
    for(i in 1:length(x)){
      input_checks(x[[i]])
    }

    for(i in 1:length(x)){

      alert_text <- paste0("Getting stats for data frame ",i,".")
      cli::cli_alert_info(alert_text)

      if(i == 1){

        map_stats <- generate_stats(x[[i]]) |>
                        dplyr::mutate(time = temporal[[i]])

      } else{

        map_stats <- rbind(map_stats,
                           generate_stats(x[[i]]) |>
                              dplyr::mutate(time = temporal[[i]]))
      }

    }

  } else { # temporal argument NOT provided
    input_checks(x)
    map_stats <- generate_stats(x)
  }

  return(map_stats)

}




