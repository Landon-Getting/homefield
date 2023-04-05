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
#' @param continental (Boolean required): If TRUE (default), only considers the continental United States. If FALSE, also considers Alaska and Hawaii.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{cfb_map_stats <- get_map_stats(get_cfb_undefeated(season = 2021, week = 6))}
get_map_stats <- function(x , continental = TRUE){


  # input data checks ----------------------------------------------------------
  # setting expected column names
  expected_cols <- c("identifier",
                     "lat",
                     "lng",
                     "color",
                     "image")

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

  if(continental == TRUE){
    STUSPS_filter <- c("VI", "PR", "GU", "AS", "MP", "UM", "AK", "HI")
  } else{
    STUSPS_filter <- c("VI", "PR", "GU", "AS", "MP", "UM")
  }

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
  map_stats <- territory_map_df |>
    dplyr::group_by(.data$identifier) |>
    dplyr::summarise(land = sum(ALAND, na.rm = TRUE),
                     water = sum(AWATER, na.rm = TRUE),
                     domain = sum(ALAND, na.rm = TRUE) + sum(AWATER, na.rm = TRUE),
                     pop = sum(population, na.rm = TRUE))

  return(map_stats)

}




