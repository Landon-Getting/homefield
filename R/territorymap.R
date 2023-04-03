#' territorymap
#'
#' @param x data frame with specific columns TBD
#' @param threshold area in km^2 for secondary logos
#' @param output_file file path for output .png
#'
#' @export
#'
#' @examples
#' cfb_data <- get_cfb_undefeated(season = 2016, week = 6)
#' territorymap(x = cfb_data, output_file = "C:/Users/darthvader/Downloads")
territorymap <- function(x, threshold = 10000, output_file){

  # TO DO
  # fix save widget location
  # add title/credit option
  # perform checks on input arguments


  # convert threshold from km^2 to m^2
  threshold <- threshold*1e6

  input_df <- x


  input_df_location <- input_df |>
    dplyr::select(identifier,lat,lng)

  # converting lat/long to sf point object
  input_df_location <-  sf::st_as_sf(input_df_location,
                                  coords = c("lng", "lat"),
                                  crs = 4326) |>
    dplyr::rename(location = geometry)

  cli::cli_alert_info("Querying Map Data...")

  states <- tigris::states(cb = TRUE) |>
    dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |>
    tigris::shift_geometry() |>
    sf::st_transform("+proj=longlat +datum=WGS84") |>
    suppressMessages()

  # less detailed county data for determining color
  counties <- tigris::counties(cb = TRUE) |>
    dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |> # filtering out territories
    sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
    dplyr::mutate(FIPS = paste0(STATEFP,COUNTYFP)) |> # add unique county identifier
    suppressMessages()

  counties$centroid <- sf::st_centroid(counties$geometry)

  # Calculating Distance between every county and identifier -----------------------
  cli::cli_alert_info("Calculating distances...")

  comparing_distances <- tidyr::expand_grid(input_df_location,
                                            counties |>
                                              dplyr::select(FIPS, centroid) |>
                                              sf::st_drop_geometry())

  comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
  comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

  comparing_distances$distances <- sf::st_distance(comparing_distances$location$x,
                                                   comparing_distances$centroid$x,
                                                   by_element = TRUE)

  comparing_distances <- comparing_distances |>
    dplyr::select(FIPS,identifier,distances) |>
    dplyr::group_by(FIPS) |>
    dplyr::slice(which.min(distances))

  # Create data frame for color mapping -----------------------------------------
  cli::cli_alert_info("Creating data frame for color mapping...")

  territory_map_df <- comparing_distances |>
    dplyr::select(FIPS,identifier) |>
    dplyr::left_join(counties |>
                       tigris::shift_geometry() |> # shift Alaska and Hawaii below
                       sf::st_transform("+proj=longlat +datum=WGS84") |>
                       dplyr::select(FIPS, geometry), by = "FIPS", keep = FALSE) |>
    dplyr::left_join(input_df |>
                       dplyr::select(identifier,color,image), by = "identifier", keep = FALSE)

  territory_map_df <- sf::st_as_sf(territory_map_df)

  # getting image locations --------------------------------------------------
  cli::cli_alert_info("Getting image locations...")

  # Apply the function to the example data frame
  counties_grouped <- territory_map_df |>
    dplyr::group_by(identifier) |>
    dplyr::summarise(geometry = suppressWarnings(sf::st_union(geometry,
                                                              is_coverage = TRUE)))  # reduces calculation time

  counties_grouped <- sf::st_cast(counties_grouped |> dplyr::select(identifier, geometry),
                                  "MULTIPOLYGON")

  counties_grouped <- sf::st_cast(counties_grouped |> dplyr::select(identifier, geometry),
                                  "POLYGON") |> suppressWarnings()

  counties_grouped <- sf::st_as_sf(counties_grouped)

  # if there is more than one logo, find area of each polygon
  # biggest polygon stays, the rest must be over a certain size to remain
  counties_grouped <- counties_grouped |>
    dplyr::mutate(area = as.numeric(sf::st_area(counties_grouped$geometry)),
                  id = dplyr::row_number())

  largest_areas <- counties_grouped |>
    dplyr::group_by(identifier) |>
    dplyr::filter(area == max(area))

  above_threshold_areas <- counties_grouped |>
    dplyr::group_by(identifier) |>
    dplyr::filter(area > threshold)

  image_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(identifier) |>
    dplyr::mutate(centroid = sf::st_centroid(geometry)) |>
    dplyr::left_join(input_df |> dplyr::select(identifier, image),
                     by = "identifier") |>
    dplyr::arrange(identifier)

  # Create logos for map --------------------------------------------------------
  cli::cli_alert_info("Creating logos for map...")

  # magic to adjust the icon size based on territory area
  icon_size <- (as.numeric((image_areas$area/1e11)) + 1) * 45

  icon_size <- ifelse(icon_size > 400, 400, icon_size)

  logoIcons <- leaflet::icons(
    iconUrl = image_areas$image,
    iconWidth = icon_size,
    iconHeight = icon_size
  )

  image_areas <- sf::st_drop_geometry(image_areas)
  image_areas <- sf::st_as_sf(image_areas)


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
                          color = "grey",
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

  text_to_plot <- tibble::tibble(x = 0.6,
                                 y = 0.9,
                                 text = paste0("Closest Undefeated D1 Football Team to each US County: \n",
                                               season,
                                               " Season, Week ",
                                               week,
                                               "\n Created by u/ThatGuy_Sev"))

  final_img <- ggplot2::ggplot(data = text_to_plot) +
    ggplot2::annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text),
                       size = 1500,
                       family = "Open Sans Extrabold",
                       fontface = "bold") +
    #ggimage::geom_image(ggplot2::aes(image = "./inst/www/cfb-imp-map-logo-named-alt.png", x = 0.085, y = 0.85), size = 0.090, asp = 1.6) +
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

