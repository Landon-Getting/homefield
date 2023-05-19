#' homefield_map
#'
#' @description Generates a homefield map and saves as a .png file at a
#' specified location.
#'
#' @param x Data frame created by other homefield functions or including the
#' following columns:\cr
#' \cr
#' \strong{entity} - identifies each entity (ex. school name - Iowa State,
#' Minnesota, Bowling Green).\cr
#' \cr
#' \strong{lat} - latitude of entity\cr
#' \cr
#' \strong{lng} - longitude of entity\cr
#' \cr
#' \strong{color} - hexadecimal color to fill entity territories (ex. #cfab7a).\cr
#' \cr
#' \strong{image} - image url or local file path to be placed on at least one
#' territory and all territories larger than the threshold.\cr
#' \cr
#' Columns must also be of equal length and match the specified names exactly.
#' @param threshold (Numeric required): If territory above threshold (area in km^2),
#' a secondary logo will be generated for that territory. \emph{Defaults to 10,000 km^2.}
#' @param output_file (String required): Local file path ending in \emph{.png}.
#' @param title (String required): Title of the map.
#' @param credit (String required): Name of the map author.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{cfb_data <- cfb_undefeated(season = 2016, week = 6)}
#' \dontrun{homefield_map(x = cfb_data, output_file = paste0(getwd(),"/homefield_map_example.png"))}
homefield_map <- function(x, threshold = 10000, output_file, title = NULL, credit = NULL){

  # passes CMD check
  image <- entity <- lat <- lng <- geometry <- STUSPS <- GEOID <- centroid <- NULL
  distances <- color <- image <- area <- id <- NULL

  # Performing input checks ---------------------------------------------------

  # setting expected column names
  expected_cols <- c("entity",
                     "lat",
                     "lng",
                     "color",
                     "image")

  # checking if column names match expected names
  if (!all(expected_cols %in% names(x))) {
    stop("x must contain columns entity, lat, lng, color, and image.")
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

  cli::cli_h1("Generating homefield map...")

  # convert threshold from km^2 to m^2
  threshold <- threshold*1e6

  # subsetting input dataframe
  input_df_location <- x |>
    dplyr::select(entity,
                  lat,
                  lng)

  # converting lat/long to sf point object -------------------------------------
  input_df_location <-  sf::st_as_sf(input_df_location,
                                  coords = c("lng", "lat"),
                                  crs = 4326) |>
    dplyr::rename(location = geometry)

  cli::cli_alert_info("Getting map data...")

  # getting map data stored internally
  states <- get0("states", envir = asNamespace("homefield")) |>
    tigris::shift_geometry() |> # shifting Alaska and Hawaii south
    sf::st_transform("+proj=longlat +datum=WGS84") # Reproject to WGS84

  counties <- get0("counties", envir = asNamespace("homefield")) |>
    sf::st_transform("+proj=longlat +datum=WGS84") # Reproject to WGS84

  counties$centroid <- sf::st_centroid(counties$geometry)

  # Calculating Distance between every county and entity -----------------------
  cli::cli_alert_info("Calculating distances...")

  # creating matrix with every county and entity
  comparing_distances <- tidyr::expand_grid(input_df_location,
                                            counties |>
                                              dplyr::select(GEOID, centroid) |>
                                              sf::st_drop_geometry())

  # converting to sf objects
  comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
  comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

  # calculating distances
  comparing_distances$distances <- sf::st_distance(comparing_distances$location$x,
                                                   comparing_distances$centroid$x,
                                                   by_element = TRUE)

  # filtering for shortest distance
  comparing_distances <- comparing_distances |>
    dplyr::select(GEOID,
                  entity,
                  distances) |>
    dplyr::group_by(GEOID) |>
    dplyr::slice(which.min(distances))

  # Create data frame for color mapping -----------------------------------------
  cli::cli_alert_info("Adding color to the mapping...")

  # creating comprehensive df for plotting
  homefield_map_df <- comparing_distances |>
    dplyr::select(GEOID,
                  entity) |>
    dplyr::left_join(counties |>
                       tigris::shift_geometry() |> # shift Alaska and Hawaii below
                       sf::st_transform("+proj=longlat +datum=WGS84") |>
                       dplyr::select(GEOID,
                                     geometry),
                                     by = "GEOID",
                                     keep = FALSE) |>
    dplyr::left_join(x |> # adding in input df values
                       dplyr::select(entity,
                                     color,
                                     image)|>
                       dplyr::distinct(),
                     by = "entity",
                     keep = FALSE)

  # converting to sf object
  homefield_map_df <- sf::st_as_sf(homefield_map_df)

  # getting image locations --------------------------------------------------
  cli::cli_alert_info("Getting image locations...")

  # combining counties into territories
  counties_grouped <- homefield_map_df |>
    dplyr::group_by(entity) |>
    dplyr::summarise(geometry = suppressWarnings(sf::st_union(geometry,
                                                              is_coverage = TRUE)))  # reduces calculation time

  # converting multipolygons to polygons
  counties_grouped <- sf::st_cast(counties_grouped |>
                                    dplyr::select(entity,
                                                  geometry),
                                  "MULTIPOLYGON")

  counties_grouped <- sf::st_cast(counties_grouped |>
                                    dplyr::select(entity,
                                                  geometry),
                                  "POLYGON") |>
                                  suppressWarnings()

  counties_grouped <- sf::st_as_sf(counties_grouped)

  # if there is more than one image, find area of each territory
  # largest territory is kept, other territories must be over a
  # certain size to remain

  # calculating area of territories
  counties_grouped <- counties_grouped |>
    dplyr::mutate(area = as.numeric(sf::st_area(counties_grouped$geometry)),
                  id = dplyr::row_number())

  # determining largest areas
  largest_areas <- counties_grouped |>
    dplyr::group_by(entity) |>
    dplyr::filter(area == max(area))

  # checking other territories to see if they are larger than threshold
  above_threshold_areas <- counties_grouped |>
    dplyr::group_by(entity) |>
    dplyr::filter(area > threshold)

  # creating df with final territories receiving an image
  image_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(entity) |>
    dplyr::mutate(centroid = sf::st_centroid(geometry)) |>
    dplyr::left_join(x |> dplyr::select(entity,
                                        image),
                     by = "entity") |>
    dplyr::arrange(entity)

  # Create logos for map --------------------------------------------------------
  cli::cli_alert_info("Adding images to map...")

  # finding the largest inscribed circle for each territory with image
  best_center <- sf::st_as_sf(geomander::st_circle_center(image_areas)) |>
    sf::st_transform("+proj=longlat +datum=WGS84")

  # adding center of inscribed circle to image_areas
  image_areas$best_center <- best_center$geometry

  # calculating radius of inscribed circle
  image_areas$distance <- sf::st_length(sf::st_nearest_points(image_areas$geometry,
                                                              image_areas$best_center,
                                                               pairwise = TRUE))

  # converting center of inscribed circle to lat/lng
  image_areas$best_center_lng <- sf::st_coordinates(image_areas$best_center)[,1]
  image_areas$best_center_lat <- sf::st_coordinates(image_areas$best_center)[,2]

  # magic to adjust the icon size based on size of inscribed circle
  icon_size <- as.numeric(image_areas$distance/1000)

  # creating icons
  logoIcons <- leaflet::icons(
    iconUrl = image_areas$image,
    iconWidth = icon_size,
    iconHeight = icon_size
  )

  image_areas <- sf::st_drop_geometry(image_areas)
  image_areas <- sf::st_as_sf(image_areas)

  # function to calculate a contrasting color for borders
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

  # determining contrasting borders for each territory
  homefield_map_df <- homefield_map_df |>
    dplyr::mutate(border_color = contrast_color(color))

  # reprojecting
  epsg2163 <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "ESRI:102003",
    proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    resolutions = 2^(16:7)
  )

  # Plotting map ----------------------------------------------------------------
  cli::cli_alert_info("Plotting map...")

  # creating leaflet map
    # setting map size and projection
  m <- leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                                          zoomControl = TRUE,
                                                          zoomSnap = 0.25,
                                                          zoomDelta = 1),
                        height = 2000,
                        width = 3200) |>
    # setting map view
    leaflet::setView(lng = -98.64580,
                     lat = 38.05909,
                     zoom = 5) |>
    # adding territories
    leaflet::addPolygons(data = homefield_map_df,
                         fillColor = ~color,
                         fillOpacity = 0.9,
                         smoothFactor = 0.2,
                         color = ~border_color,
                         weight = 0.25,
                         opacity = 0.75,
                         stroke = T,
                         label = ~entity) |>
    # adding images
    leaflet::addMarkers(data = image_areas,
                        lat = image_areas$best_center_lat,
                        lng = image_areas$best_center_lng,
                        label = ~entity,
                        icon = logoIcons) |>
    # adding state borders
    leaflet::addPolylines(data = states,
                          color = "black",
                          weight = 1,
                          smoothFactor = 0,
                          opacity = 1) |>
    # adding white background
    leaflet.extras::setMapWidgetStyle(list(background= "white"))


  # Saving the PNG -------------------------------------------------------------
  cli::cli_alert_info("Saving as .png...")

  # saving map as widget
  htmlwidgets::saveWidget(m, paste0(dirname(output_file), "/homefield_temp.html"), selfcontained = F)

  # taking a screenshot of the map widget
  webshot2::webshot(url = paste0(dirname(output_file), "/homefield_temp.html"),
                    file = output_file,
                    vwidth = 3200,
                    vheight = 2000)


  # Adding text, logo, etc to the screenshot
  img <- png::readPNG(output_file)
  h <- as.numeric(dim(img)[1])
  w <- as.numeric(dim(img)[2])


  if(is.null(credit)){
    credit_text <- ""
  } else{
    credit_text <- paste0("Created by ",
                          credit,
                          " with the homefield R package.")
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
                       size = 1600,
                       fontface = "bold") +
    ggplot2::geom_text(ggplot2::aes(x = 0.5,
                                    y = 0.03,
                                    label = credit_text),
                       size = 800) +
    # hex sticker
    ggimage::geom_image(ggplot2::aes(image = "https://github.com/Landon-Getting/homefield-graphics/blob/main/sticker.png?raw=true",
                                     x = 250/3200, # 250  pixels from the left
                                     y = 1-(215/2000)), # 225 pixels from the top
                        size = 0.090,
                        asp = 1.6) +
    ggplot2::xlim(0,1) +
    ggplot2::ylim(0,1) +
    ggthemes::theme_map()

  # Saving screenshot
  ggplot2::ggsave(paste0(output_file),
                  plot = final_img,
                  device = "png",
                  width = w,
                  height = h,
                  limitsize = FALSE,
                  dpi = 1)

  # trimming screenshot
  magick::image_read(output_file) |>
    magick::image_trim() |>
    magick::image_chop("40x10") |>
    magick::image_flip() |>
    magick::image_chop("10x20") |>
    magick::image_flip() |>
    magick::image_write(output_file, format = "png")

  # removing temporary widget file
  file.remove(paste0(dirname(output_file), "/homefield_temp.html"))

  cli::cli_alert_info("Finished! :)")

}
