library(tigris)
library(sf)
library(leaflet)
library(magrittr)
library(dplyr)
library(magick)

# testing parameters
week = 12
season = 2021
output_file = "C:/Users/lwget/downloads/territorymap.png"
threshold = 1e10

### TO DO
# Think about creating saved object with counties and distance calculations already performed
# Improve logo size generation - convert area to pixels

territorymap <- function(season = 2021, week = 6, threshold = 1e10, output_file){

  start_time <- Sys.time()

  cli::cli_h1("Generating CFB Undefeated map...")

  # Querying CFB Data ----------------------------------------------------------
  cli::cli_alert_info("Querying CFB Data...")

  if(week == 0){

    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

  } else{

    for(week_number in 1:week){

      if(week_number == 1){
        game_info <- cfbfastR::cfbd_game_info(year = season,
                                              week = week_number)

      } else{
        game_info <- rbind(game_info, cfbfastR::cfbd_game_info(year = season,
                                                               week = week_number))
      }
    }

    losers <- game_info |>
      dplyr::mutate(
        winner = dplyr::if_else(home_points > away_points, home_id, away_id),
        loser = dplyr::if_else(home_points < away_points, home_id, away_id)
      ) |>
      dplyr::select(loser) |>
      unique()

    teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = season)

    teams <- teams[!(teams$team_id %in% losers$loser), ]
  }

  # getting only the columns needed
  teams_location <- teams |>
    dplyr::select(team_id,latitude,longitude)

  # converting lat/long to sf point object
  teams_location <-  sf::st_as_sf(teams_location,
                                     coords = c("longitude", "latitude"),
                                     crs = 4326) |>
                     dplyr::rename(location = geometry)


  # Querying Map Data ----------------------------------------------------------
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

  # Calculating Distance between every county and school -----------------------
  cli::cli_alert_info("Calculating distances...")

  comparing_distances <- tidyr::expand_grid(teams_location,
                                            counties |>
                                              dplyr::select(FIPS, centroid) |>
                                              sf::st_drop_geometry())

  comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
  comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

  comparing_distances$distances <- sf::st_distance(comparing_distances$location$x,
                                                   comparing_distances$centroid$x,
                                                   by_element = TRUE)

  comparing_distances <- comparing_distances |>
    dplyr::select(FIPS,team_id,distances) |>
    dplyr::group_by(FIPS) |>
    dplyr::slice(which.min(distances))

  # Determine best colors and logos to use for the map ---------------------------
  cli::cli_alert_info("Deciding on best colors and logos...")

  styling <- read.csv("C:/Users/lwget/Downloads/styling.csv")

  # getting best colors
  teams <- teams |>
    dplyr::mutate(best_color = dplyr::case_when(
      school %in% styling$alt_color_list ~ alt_color,
      !school %in% styling$alt_color_list ~ color
    ))

  # getting best logos
  teams <- teams |>
    dplyr::mutate(best_logo = dplyr::case_when(
      school %in% styling$alt_logo_list ~ logo_2,
      !school %in% styling$alt_logo_list ~ logo
    ))

  teams <- teams |> dplyr::arrange(school)

  # Create data frame for color mapping -----------------------------------------
  cli::cli_alert_info("Creating data frame for color mapping...")

  school_map <- comparing_distances |>
    dplyr::select(FIPS,team_id) |>
    dplyr::left_join(counties |>
                       tigris::shift_geometry() |> # shift Alaska and Hawaii below
                       sf::st_transform("+proj=longlat +datum=WGS84") |>
                       dplyr::select(FIPS, geometry), by = "FIPS", keep = FALSE) |>
    dplyr::left_join(teams |>
                       dplyr::select(team_id, school,best_color, best_logo), by = "team_id", keep = FALSE)

  school_map <- sf::st_as_sf(school_map)

  # getting logo locations --------------------------------------------------
  cli::cli_alert_info("Getting logo locations...")

  # Apply the function to the example data frame
  counties_grouped <- school_map |>
    dplyr::group_by(school) |>
    dplyr::summarise(geometry = suppressWarnings(sf::st_union(geometry,
                                             is_coverage = TRUE)))  # reduces calculation time

  counties_grouped <- sf::st_cast(counties_grouped |> dplyr::select(school, geometry),
                       "MULTIPOLYGON")

  counties_grouped <- sf::st_cast(counties_grouped |> dplyr::select(school, geometry),
                       "POLYGON") |> suppressWarnings()

  counties_grouped <- sf::st_as_sf(counties_grouped)

  # if there is more than one logo, find area of each polygon
  # biggest polygon stays, the rest must be over a certain size to remain
  counties_grouped <- counties_grouped |>
    dplyr::mutate(area = as.numeric(sf::st_area(counties_grouped$geometry)),
                  id = dplyr::row_number())

  largest_areas <- counties_grouped |>
    dplyr::group_by(school) |>
    dplyr::filter(area == max(area))

  above_threshold_areas <- counties_grouped |>
    dplyr::group_by(school) |>
    dplyr::filter(area > threshold)

  logo_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(school) |>
    dplyr::mutate(centroid = sf::st_centroid(geometry)) |>
    dplyr::left_join(teams |> dplyr::select(school, best_logo),
                     by = "school") |>
    dplyr::arrange(school)

  # Create logos for map --------------------------------------------------------
  cli::cli_alert_info("Creating logos for map...")

  # magic to adjust the icon size based on territory area
  #icon_size <- (as.numeric(log(logo_areas$area)) - 21) * 22
  icon_size <- (as.numeric((logo_areas$area/1e11)) + 1) * 45

  icon_size <- ifelse(icon_size > 400, 400, icon_size)

  logoIcons <- leaflet::icons(
    iconUrl = logo_areas$best_logo,
    iconWidth = icon_size,
    iconHeight = icon_size
  )

  logo_areas <- sf::st_drop_geometry(logo_areas)
  logo_areas <- sf::st_as_sf(logo_areas)


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
    leaflet::addPolygons(data = school_map,
                         color = "#434445",
                         weight = 0,
                         fillColor = ~best_color,
                         fillOpacity = 0.9,
                         smoothFactor = 0.2,
                         stroke = F,
                         label = ~school) |>
    leaflet::addMarkers(data = logo_areas,
                        label = ~school,
                        icon = logoIcons) |>
    leaflet::addPolylines(data = school_map,
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

  htmlwidgets::saveWidget(m, "C:/Users/lwget/downloads/temp.html", selfcontained = F)

  webshot2::webshot(url = "C:/Users/lwget/downloads/temp.html",
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

  cli::cli_alert_info("Finished! :)")

  end_time <- Sys.time()

  total_time <- end_time - start_time

  return(sprintf("The program ran for %s seconds.", total_time))

}

territorymap(season = 2017,
             week = 0,
             output_file = "C:/Users/lwget/downloads/territorymap.png")


