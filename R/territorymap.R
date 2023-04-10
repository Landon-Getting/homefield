#' territorymap
#'
#' @description Generates a territory map and saves as a .png file at a specified location.
#'
#' @param x Data frame created by other territorymap functions or including the following columns:\cr
#' \cr
#' \strong{identifier} - identifies each element (ex. school name - Iowa State, Minnesota, Bowling Green).\cr
#' \cr
#' \strong{lat} - latitude of element.\cr
#' \cr
#' \strong{lng} - longitude of element.\cr
#' \cr
#' \strong{color} - hexadecimal color to fill element territories (ex. #cfab7a).\cr
#' \cr
#' \strong{image} - image url or local file path to be placed on at least one territory and all territories over the threshold.\cr
#' \cr
#' Columns must also be of equal length and match the specified names exactly.
#' @param threshold (Numeric required): If territory above threshold (area in km^2), a secondary logo will be generated for that territory. \emph{Defaults to 10,000 km^2.}
#' @param output_file (String required): Local file path ending in \emph{.png}.
#' @param title (String required): Title of the map.
#' @param credit (String required): Name of the map author.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{cfb_data <- get_cfb_undefeated(season = 2016, week = 6)}
#' \dontrun{territorymap(x = cfb_data, output_file = paste0(getwd(),"/territorymap_example.png"))}
territorymap <- function(x, threshold = 10000, output_file, title = NULL, credit = NULL){

  # TO DO
  # improve logo size generation
  # what if no undefeated

  # Performing input checks ---------------------------------------------------

  # setting expected column names
  expected_cols <- c("identifier",
                     "lat",
                     "lng",
                     "color",
                     "image")

  # checking if column names match expected names
  if (!all(expected_cols %in% names(x))) {
    stop("x must contain columns identifier, lat, lng, color, and image.")
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

  # Defining a regular expression for matching valid hex color codes
  hex_regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

  # Checking if the color column contains valid hex color codes
  if (!all(grepl(hex_regex, x$color))) {
    stop("x must contain a color column with valid colors in hexadecimal format.")
  }

  # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
  # creating url checker for image column
  valid_url <- function(url_in,t=2){
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
    suppressWarnings(try(close.connection(con),silent=T))
    ifelse(is.null(check),TRUE,FALSE)
  }

  # if starts with http, check if valid URL
  if(all(sapply(x$image, function(x) substr(x, 1, 4) == "http"))) {
    if(!(all(sapply(x$image, valid_url)))) {
      stop("x must contain an image column with valid urls or accessible local file paths.")
    }
  }

  # checking image column for valid url or file paths
  if((all(dir.exists(x$image)) & all(file.access(x$image, mode = 4)))) {
    stop("x must contain an image column with valid urls or accessible local file paths.")
  }

  # checking valid threshold value
  if (!(threshold > 0 & is.numeric(threshold))) {
    stop("The threshold must be a positive numerical value representing area in km^2.")
  }

  # checking if output_file path is valid and user has write permission
  if (!dir.exists(dirname(output_file)) | file.access(dirname(output_file), mode = 2)) {
    stop("output_file must be a valid directory path with write access.")
  }

  # Checking if output_file basename is valid
  if (!grepl("^[^[:cntrl:]/?*:;{}\\\\]+\\.[^[:cntrl:]/?*:;{}\\\\]+$", basename(output_file))) {
    stop("Invalid output_file name.")
  }

  # Checking if title is valid
  if(!missing(title)){
    if (!is.character(title)) {
      stop("title must be of type character.")
    }
  }

  # Checking if credit is valid
  if(!missing(credit)){
    if (!is.character(credit)) {
      stop("credit must be of type character.")
    }
  }

  cli::cli_h1("Generating Territory Map...")

  # convert threshold from km^2 to m^2
  threshold <- threshold*1e6

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

  cli::cli_alert_info("Querying Map Data...")

  states <- tigris::states(cb = TRUE, progress_bar = FALSE) |>
    dplyr::filter(!.data$STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |>
    tigris::shift_geometry() |>
    sf::st_transform("+proj=longlat +datum=WGS84") |>
    suppressMessages()

  counties <- tigris::counties(cb = TRUE, progress_bar = FALSE) |>
    dplyr::filter(!.data$STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |> # filtering out territories
    sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
    suppressMessages()

  counties$centroid <- sf::st_centroid(counties$geometry)

  # Calculating Distance between every county and identifier -----------------------
  cli::cli_alert_info("Calculating distances...")

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

  # Create data frame for color mapping -----------------------------------------
  cli::cli_alert_info("Creating data frame for color mapping...")

  territory_map_df <- comparing_distances |>
    dplyr::select(.data$GEOID,
                  .data$identifier) |>
    dplyr::left_join(counties |>
                       tigris::shift_geometry() |> # shift Alaska and Hawaii below
                       sf::st_transform("+proj=longlat +datum=WGS84") |>
                       dplyr::select(.data$GEOID,
                                     .data$geometry),
                                     by = "GEOID",
                                     keep = FALSE) |>
    dplyr::left_join(input_df |>
                       dplyr::select(.data$identifier,
                                     .data$color,
                                     .data$image)|>
                       dplyr::distinct(),
                     by = "identifier",
                     keep = FALSE)

  territory_map_df <- sf::st_as_sf(territory_map_df)

  # getting image locations --------------------------------------------------
  cli::cli_alert_info("Getting image locations...")

  # Apply the function to the example data frame
  counties_grouped <- territory_map_df |>
    dplyr::group_by(.data$identifier) |>
    dplyr::summarise(geometry = suppressWarnings(sf::st_union(.data$geometry,
                                                              is_coverage = TRUE)))  # reduces calculation time

  counties_grouped <- sf::st_cast(counties_grouped |>
                                    dplyr::select(.data$identifier,
                                                  .data$geometry),
                                  "MULTIPOLYGON")

  counties_grouped <- sf::st_cast(counties_grouped |>
                                    dplyr::select(.data$identifier,
                                                  .data$geometry),
                                  "POLYGON") |>
                                  suppressWarnings()

  counties_grouped <- sf::st_as_sf(counties_grouped)

  # if there is more than one logo, find area of each polygon
  # biggest polygon stays, the rest must be over a certain size to remain
  counties_grouped <- counties_grouped |>
    dplyr::mutate(area = as.numeric(sf::st_area(counties_grouped$geometry)),
                  id = dplyr::row_number())

  largest_areas <- counties_grouped |>
    dplyr::group_by(.data$identifier) |>
    dplyr::filter(.data$area == max(.data$area))

  above_threshold_areas <- counties_grouped |>
    dplyr::group_by(.data$identifier) |>
    dplyr::filter(.data$area > threshold)

  image_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
    dplyr::distinct(.data$id, .keep_all = TRUE) |>
    dplyr::arrange(.data$identifier) |>
    dplyr::mutate(centroid = sf::st_centroid(.data$geometry)) |>
    dplyr::left_join(input_df |> dplyr::select(.data$identifier,
                                               .data$image),
                     by = "identifier") |>
    dplyr::arrange(.data$identifier)

  # Create logos for map --------------------------------------------------------
  cli::cli_alert_info("Creating logos for map...")

  # magic to adjust the icon size based on territory area
  icon_size <- (as.numeric((image_areas$area/1e11)) + 1) * 43

  icon_size <- ifelse(icon_size > 400, 400, icon_size)

  logoIcons <- leaflet::icons(
    iconUrl = image_areas$image,
    iconWidth = icon_size,
    iconHeight = icon_size
  )

  image_areas <- sf::st_drop_geometry(image_areas)
  image_areas <- sf::st_as_sf(image_areas)

  # adding contrasting border apartment
  contrast_color <- function(hex_input) {

    rgb.array <- grDevices::col2rgb(hex_input)
    r <- (rgb.array[1] ^ 2) * .068
    g <- (rgb.array[2] ^ 2) * .691
    b <- (rgb.array[3] ^ 2) * .241

    brightness <- 255 - sqrt(r + g + b)

    if(brightness > 80 & brightness < 128){
      brightness <- 80
    } else if(brightness > 128 & brightness < 240){
      brightness <- 240
    }

    hex_output <- grDevices::rgb(brightness, brightness, brightness, maxColorValue=255)

    return(hex_output)
  }

  territory_map_df <- territory_map_df |>
    dplyr::mutate(border_color = contrast_color(.data$color))

  # Reprojection
  epsg2163 <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "ESRI:102003",
    proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    resolutions = 2^(16:7)
  )

  # Plotting map ----------------------------------------------------------------
  cli::cli_alert_info("Plotting map...")

  m <- leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                                          zoomControl = TRUE,
                                                          zoomSnap = 0.25,
                                                          zoomDelta = 1),
                        height = 2000,
                        width = 3200) |>
    leaflet::setView(lng = -98.64580,
                     lat = 38.05909,
                     zoom = 5) |>
    leaflet::addPolygons(data = territory_map_df,
                         color = "#434445",
                         weight = 0,
                         fillColor = ~color,
                         fillOpacity = 0.9,
                         smoothFactor = 0.2,
                         stroke = F,
                         label = ~identifier) |>
    leaflet::addMarkers(data = image_areas,
                        label = ~identifier,
                        icon = logoIcons) |>
    leaflet::addPolylines(data = territory_map_df,
                          color = ~border_color,
                          weight = 0.25,
                          smoothFactor = 0,
                          opacity = 0.75)  |>
    leaflet::addPolylines(data = states,
                          color = "black",
                          weight = 1,
                          smoothFactor = 0,
                          opacity = 1)


  # Saving the PNG -------------------------------------------------------------
  cli::cli_alert_info("Saving as .png...")

  htmlwidgets::saveWidget(m, paste0(dirname(output_file), "/territorymap_temp.html"), selfcontained = F)

  webshot2::webshot(url = paste0(dirname(output_file), "/territorymap_temp.html"),
                    file = output_file,
                    vwidth = 3200,
                    vheight = 2000)


  # Adding text, logo, etc.
  img <- png::readPNG(output_file)
  h <- as.numeric(dim(img)[1])
  w <- as.numeric(dim(img)[2])


  if(is.null(credit)){
    credit_text <- ""
  } else{
    credit_text <- paste0("Created by ",
                          credit,
                          " with the territorymap R package.")
  }

  if(is.null(title)){
    title_text <- ""
  } else{
    title_text <- title
  }

  final_img <- ggplot2::ggplot() +
    ggplot2::annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    ggplot2::geom_text(ggplot2::aes(x = 0.5,
                                    y = 0.9,
                                    label = title_text),
                       size = 1500,
                       family = "Open Sans Extrabold",
                       fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(x = 0.5,
                                    y = 0.03,
                                    label = credit_text),
                       size = 700,
                       family = "Open Sans Extrabold") +
    # hex sticker
    ggimage::geom_image(ggplot2::aes(image = "./inst/figures/sticker.png",
                                     x = 250/3200, # 250  pixels from the left
                                     y = 1-(215/2000)), # 225 pixels from the top
                        size = 0.090,
                        asp = 1.6) +
    ggplot2::xlim(0,1) +
    ggplot2::ylim(0,1) +
    ggthemes::theme_map()

  # Saving, trimming, and compressing
  ggplot2::ggsave(paste0(output_file),
                  plot = final_img,
                  device = "png",
                  width = w,
                  height = h,
                  limitsize = FALSE,
                  dpi = 1)

  magick::image_read(output_file) |>
    magick::image_trim() |>
    magick::image_chop("40x10") |>
    magick::image_flip() |>
    magick::image_chop("10x20") |>
    magick::image_flip() |>
    magick::image_write(output_file, format = "png")

  file.remove(paste0(dirname(output_file), "/territorymap_temp.html"))

  cli::cli_alert_info("Finished! :)")

}
