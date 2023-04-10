#' get_map_stats
#' @description Returns a data frame with summary statistics for each identifier based on their map territory. \cr
#' \cr \strong{This function requires a CENSUS API Key.}
#' Follow the directions \href{https://walker-data.com/tidycensus/reference/census_api_key.html}{here}
#' to receive a key.
#'
#' @returns Output data frame includes four summary statistics:\cr
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
#' @param x (Required): Data frame created by other territorymap functions or including the following columns:\cr
#' \cr
#' \strong{identifier} - identifies each element (ex. school name - Iowa State, Minnesota, Bowling Green).\cr
#' \cr
#' \strong{lat} - latitude of element.\cr
#' \cr
#' \strong{lng} - longitude of element.\cr
#' \cr
#' \strong{color} - \emph{(Optional:)} hexadecimal color (ex. #cfab7a) useful for plotting with territorymap(). Set keep_visuals = TRUE to include this column in the returned data frame.\cr
#' \cr
#' \strong{image} - \emph{(Optional:)}image url or local file path useful for plotting with territorymap(). Set keep_visuals = TRUE to include this column in the returned data frame.\cr
#' \cr
#' Columns must also be of equal length and match the specified names exactly.
#'
#' @param continental (Optional - Boolean required): \cr
#' \cr If TRUE (default), only considers the continental United States.\cr
#' \cr If FALSE, also considers Alaska and Hawaii.
#'
#' @param temporal (Optional - Vector of date-time values required):
#' This will alter the returned data frame to include an extra column named 'time'
#' with the supplied temporal information in a tidy format.\cr
#' \cr The vector should contain temporal values representing the respective time
#' of each data frame. If argument declared with a vector, x must be a list of data frames.
#' The length of the vector supplied must equal the number of data frames in x.
#'
#' @param keep_max (Optional - Boolean required:) In some cases, a identifier may no longer be present in future temporal instances.\cr
#' \cr If keep_max = TRUE, identifiers will be generated for future instances and their statistics will match the instance where they were most recently present.\cr
#' \cr If keep_max = FALSE (default), identifiers will be generated in future instances but their statistics will be set to 0.
#'\cr If keep_max = NULL, identifiers will NOT be generated in future instances where they do not appear already.
#'
#' @param keep_visuals (Optional - Boolean required:) If x has color and image columns present,
#' keep_visuals can be used to discard or keep these columns in the returned data frame.\cr
#' \cr If keep_visuals = TRUE, the columns will be kept.\cr
#' \cr If keep_visuals = FALSE (default), the columns will be removed.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # standard example
#' cfb_map_stats <- get_map_stats(get_cfb_undefeated(season = 2021, week = 6))
#'
#' # temporal example
#' x_input <- list(get_cfb_undefeated(season = 2021, week = 0),
#'                 get_cfb_undefeated(season = 2021, week = 1),
#'                 get_cfb_undefeated(season = 2021, week = 2),
#'                 get_cfb_undefeated(season = 2021, week = 3),
#'                 get_cfb_undefeated(season = 2021, week = 4))
#'
#' # week 0 through 4
#' temporal_input <- lubridate::ymd(c("2021-08-28",
#'                                    "2021-09-04",
#'                                    "2021-09-11",
#'                                    "2021-09-18"))
#'
#' map_stats <- territorymap::get_map_stats(x = x_input,
#'                                          temporal = temporal_input)
#'
#'}
get_map_stats <- function(x,
                          continental = TRUE,
                          temporal = NULL,
                          keep_max = FALSE,
                          keep_visuals = FALSE){

  if(!missing(keep_max) & !is.logical(keep_max)){
    stop("temporal must be a boolean value.")
  }

  if(!missing(keep_max) & missing(temporal)){
    warning("The keep_max argument will only be used if the temporal argument is also used.")
  }

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

    visual_check = FALSE

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

    # keep_visuals = TRUE, check the visuals
    if(keep_visuals == TRUE){

      if(!"color" %in% colnames(x) | !"image" %in% colnames(x)){
        stop("color and image columns must be present in x since keep_visuals = TRUE.")
      }

      # Defining a regular expression for matching valid hex color codes
      hex_regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

      # Checking if the color column contains valid hex color codes
      if (!all(grepl(hex_regex, x$color))) {
        warning("x should contain a color column with valid colors in hexadecimal format.")
      }

      # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
      # creating url checker for image column
      valid_url <- function(url_in,t=2){
        con <- url(url_in)
        check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
        suppressWarnings(try(close.connection(con),silent=T))
        ifelse(is.null(check),TRUE,FALSE)
      }

      # checking image column for valid url or file paths
      if(!(all(sapply(x$image,valid_url)) | (all(dir.exists(x$image)) & all(file.access(x$image, mode = 4))))) {
        warning("x should contain an image column with valid urls or accessible local file paths.")
      }

      visual_check = TRUE
    }

    return(visual_check)

  }

  generate_stats <- function(input_df, visual_check){

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

    territory_map_df <- territory_map_df |>
      dplyr::group_by(.data$identifier) |>
      dplyr::summarise(land = sum(.data$ALAND, na.rm = TRUE),
                       water = sum(.data$AWATER, na.rm = TRUE),
                       domain = sum(.data$ALAND, na.rm = TRUE) + sum(.data$AWATER, na.rm = TRUE),
                       pop = sum(.data$population, na.rm = TRUE))

    # keep_visuals = TRUE and checks pass, add color and image back in
    if(visual_check == TRUE){
      territory_map_df <- dplyr::left_join(territory_map_df,
                                           input_df |> dplyr::select(.data$identifier,
                                                                     .data$color,
                                                                     .data$image),
                                           by = "identifier")
    }


    return(territory_map_df)
  }

  # temporal? ----------------------------------------------------------------

  if(!missing(temporal)){ # temporal argument provided

    # check to ensure length of temporal matches length of x
    if(length(temporal) != length(x)){
      stop("The length of temporal must match the number of data frames in x.")
    }

    # if temporal present and not a date-time vector, return error
    if(!all(sapply(temporal, lubridate::is.timepoint))){
      stop("temporal must consist of date-time values.")
    }

    # check that temporal has unique values
    if(length(temporal) != length(unique(temporal))){
      stop("temporal must have unique values.")
    }


    for(i in 1:length(x)){

      alert_text <- paste0("Getting stats for data frame ",i,".")
      cli::cli_alert_info(alert_text)
      visual_check <- input_checks(x[[i]])

      if(i == 1){


        map_stats <- generate_stats(x[[i]], visual_check) |>
                        dplyr::mutate(time = temporal[[i]])

      } else{

        map_stats <- rbind(map_stats,
                           generate_stats(x[[i]], visual_check) |>
                              dplyr::mutate(time = temporal[[i]]))
      }

    }

    # if not present, duplicate most recent instance to the future instance(s)
    for(i in 1:length(temporal)){

      if(i == 1){
        starting_identifiers <- dplyr::filter(map_stats, .data$time == temporal[[i]])[[1]]

      } else{

        current_identifiers <- dplyr::filter(map_stats, .data$time == temporal[[i]])[[1]]

        # Find the missing elements in the current identifiers
        missing_identifiers <- setdiff(starting_identifiers, current_identifiers)

        if(keep_max == TRUE){

          # get missing rows
          missing_rows <- map_stats |>
            dplyr::filter(.data$time == temporal[[i - 1]],
                          .data$identifier %in% missing_identifiers) |>
            dplyr::mutate(time = temporal[[i]])

          # Find the most recent row of each identifier and rbind with current time
          map_stats <- rbind(map_stats,
                                  missing_rows)

        } else{

          # get missing rows
          missing_rows <- map_stats |>
            dplyr::filter(.data$time == temporal[[i - 1]],
                          .data$identifier %in% missing_identifiers) |>
            dplyr::mutate(land = 0,
                          water = 0,
                          domain = 0,
                          pop = 0,
                          time = temporal[[i]])

          # Find the most recent row of each identifier and rbind with current time
          map_stats <- rbind(map_stats,
                                  missing_rows)
        }
      }
    }

    map_stats <- map_stats |> dplyr::arrange(.data$time, .data$identifier)

    # removes duplicate rows for when identifier has multiple instances
    map_stats <- map_stats[!duplicated(map_stats[c("identifier","time")]),]

  } else { # temporal argument NOT provided
    visual_check <- input_checks(x)
    map_stats <- generate_stats(x, visual_check)
  }

  return(map_stats)

}




