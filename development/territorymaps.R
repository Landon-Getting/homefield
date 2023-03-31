library(tigris)
library(sf)
library(leaflet)
library(magrittr)
library(dplyr)
library(magick)

### TO DO
# Think about creating saved object with counties and distance calculations already performed
# Improve logo size generation - convert area to pixels

territorymap <- function(season = 2021, week = 6, threshold = 1e10, output_file){

  cli::cli_h1("Generating CFB Undefeated map...")

  # Querying CFB Data ----------------------------------------------------------
  cli::cli_alert_info("Querying CFB Data...")

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

  team_data <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = 2022)

  undefeated_teams <- team_data[!(team_data$team_id %in% losers$loser), ]

  # getting only the columns needed
  undefeated_subset <- undefeated_teams |>
    dplyr::select(team_id,latitude,longitude)

  # converting lat/long to sf point object
  undefeated_subset <-  sf::st_as_sf(undefeated_subset,
                                     coords = c("longitude", "latitude"),
                                     crs = 4326) |>
    dplyr::rename(location = geometry)


  # Querying Map Data ----------------------------------------------------------
  cli::cli_alert_info("Querying Map Data...")

  states <- tigris::states(cb = TRUE) |>
    dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |>
    tigris::shift_geometry() |>
    sf::st_transform("+proj=longlat +datum=WGS84")

  # less detailed county data for determining color
  counties <- tigris::counties(cb = TRUE) |>
    dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |> # filtering out territories
    sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
    dplyr::mutate(FIPS = paste0(STATEFP,COUNTYFP)) # add unique county identifier

  counties$centroid <- sf::st_centroid(counties$geometry)

  # more detailed county information for determining logo locations
  # counties_detailed <- tigris::counties() |>
  #   dplyr::filter(!STATEFP %in% c(72,78,69,60,66,78)) |> # filtering out territories
  #   sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
  #   dplyr::mutate(FIPS = paste0(STATEFP,COUNTYFP)) # add unique county identifier
  #
  # counties_detailed$centroid <- sf::st_centroid(counties_detailed$geometry)
  #

  # Calculating Distance between every county and school -----------------------
  cli::cli_alert_info("Calculating distances...")

  comparing_distances <- tidyr::expand_grid(undefeated_subset,
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
  undefeated_teams <- undefeated_teams |>
    dplyr::mutate(best_color = dplyr::case_when(
      school %in% styling$alt_color_list ~ alt_color,
      !school %in% styling$alt_color_list ~ color
    ))

  # getting best logos
  undefeated_teams <- undefeated_teams |>
    dplyr::mutate(best_logo = dplyr::case_when(
      school %in% styling$alt_logo_list ~ logo_2,
      !school %in% styling$alt_logo_list ~ logo
    ))

  undefeated_teams <- undefeated_teams |> dplyr::arrange(school)

  # Create data frame for color mapping -----------------------------------------
  cli::cli_alert_info("Creating data frame for color mapping...")

  master_df <- comparing_distances |>
    dplyr::select(FIPS,team_id) |>
    dplyr::left_join(counties |>
                       tigris::shift_geometry() |> # shift Alaska and Hawaii below
                       sf::st_transform("+proj=longlat +datum=WGS84") |>
                       dplyr::select(FIPS, NAME, geometry), by = "FIPS", keep = FALSE) |>
    dplyr::left_join(undefeated_teams |>
                       dplyr::select(team_id, school,best_color, best_logo), by = "team_id", keep = FALSE)

  master_df <- sf::st_as_sf(master_df)


  # Create data frame for logo mapping -------------------------------------------
  cli::cli_alert_info("Creating data frame for logo mapping...")

  # master_logo_df <- comparing_distances |>
  #   dplyr::select(FIPS,team_id) |>
  #   dplyr::left_join(counties_detailed |>
  #                      tigris::shift_geometry() |>
  #                      dplyr::filter(!STATEFP %in% c(72,78,69,60,66,78)) |>
  #                      sf::st_transform("+proj=longlat +datum=WGS84") |>
  #                      dplyr::select(FIPS, NAME, geometry), by = "FIPS", keep = FALSE) |>
  #   dplyr::left_join(undefeated_teams |>
  #                      dplyr::select(team_id, school,best_logo), by = "team_id", keep = FALSE)
  #
  #
  # master_logo_df <- sf::st_as_sf(master_logo_df)

  # getting logo locations --------------------------------------------------
  cli::cli_alert_info("Getting logo locations...")

  # Apply the function to the example data frame
  school_zones <- master_df |>
    dplyr::group_by(school) |>
    dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE))

  school_zones <- sf::st_cast(school_zones |> dplyr::select(school, geometry),
                       "MULTIPOLYGON")

  school_zones <- sf::st_cast(school_zones |> dplyr::select(school, geometry),
                       "POLYGON")

  school_zones <- sf::st_as_sf(school_zones)

  # if there is more than one logo, find area of each polygon
  # biggest polygon stays, the rest must be over a certain size to remain
  school_zones <- school_zones |>
    dplyr::mutate(area = sf::st_area(school_zones$geometry))

  school_zones$area <- as.numeric(school_zones$area)

  school_zones <- dplyr::mutate(school_zones,
                                id = dplyr::row_number())

  largest_area <- school_zones |>
    dplyr::group_by(school) |>
    dplyr::filter(area == max(area))

  filtered <- school_zones |>
    dplyr::group_by(school) |>
    dplyr::filter(area > threshold)

  result <- dplyr::bind_rows(largest_area, filtered) |>
    dplyr::distinct(id, .keep_all = TRUE) |>
    dplyr::arrange(school)

  logo_location <- result |>
    dplyr::mutate(centroid = sf::st_centroid(result$geometry))

  # NEW
  logo_location <- dplyr::left_join(logo_location,
                                    undefeated_teams |>
                                      dplyr::select(school,best_logo),
                                    by = "school")

  logo_location <- logo_location |> dplyr::arrange(school)


  # Create logos for map --------------------------------------------------------
  cli::cli_alert_info("Creating logos for map...")

  #icon_size <- (logo_location$area/5e+12)
  icon_size <- (as.numeric(log(logo_location$area)) - 21) * 22

  logoIcons <- leaflet::icons(
    iconUrl = logo_location$best_logo,
    # magic to adjust the icon size based on territory area
    iconWidth = icon_size,
    iconHeight = icon_size
  )

  logo_location <- sf::st_drop_geometry(logo_location)
  logo_location <- sf::st_as_sf(logo_location)


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
    leaflet::addPolygons(data = master_df,
                         color = "#434445",
                         weight = 0,
                         fillColor = ~best_color,
                         fillOpacity = 0.9,
                         smoothFactor = 0.2,
                         stroke = F,
                         label = ~school) |>
    leaflet::addMarkers(data = logo_location,
                        label = ~school,
                        icon = logoIcons) |>
    leaflet::addPolylines(data = master_df,
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
                                 text = paste0("CFB Undefeated Map: \n",  2021, " Season, Week ", 3))

  final_img <- ggplot2::ggplot(data = text_to_plot) +
    ggplot2::annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
    ggplot2::geom_text(ggplot2::aes(x = x, y = y, label = text),
                       size = 1800,
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

  # read in image
  img <- magick::image_read(output_file)

  # trim whitespace on edges
  img <- magick::image_trim(img)

  # shave off a few extra pixels to remove leaflet UI
  img <- magick::image_chop(img, "+40+10")

  # save trimmed image
  magick::image_write(img, output_file)

  cli::cli_alert_info("Finished! :)")

}

territorymap(season = 2021,
             week = 3,
             output_file = "C:/Users/lwget/downloads/territorymap.png")







week = 3
season = 2021
output_file = "C:/Users/lwget/downloads/territorymap.png"
threshold = 1e10

cli::cli_h1("Generating CFB Undefeated map...")

# Querying CFB Data ----------------------------------------------------------
cli::cli_alert_info("Querying CFB Data...")

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

team_data <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = 2022)

undefeated_teams <- team_data[!(team_data$team_id %in% losers$loser), ]

# getting only the columns needed
undefeated_subset <- undefeated_teams |> dplyr::select(team_id,latitude,longitude)

# converting lat/long to sf point object
undefeated_subset <-  sf::st_as_sf(undefeated_subset,
                                   coords = c("longitude", "latitude"),
                                   crs = 4326) |>
  dplyr::rename(location = geometry)


# Querying Map Data ----------------------------------------------------------
cli::cli_alert_info("Querying Map Data...")

states <- tigris::states(cb = TRUE) |>
  dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |>
  tigris::shift_geometry() |>
  sf::st_transform("+proj=longlat +datum=WGS84")


# less detailed county data for determining color
counties <- tigris::counties(cb = TRUE) |>
  dplyr::filter(!STUSPS %in% c("VI", "PR", "GU", "AS", "MP", "UM")) |> # filtering out territories
  sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
  dplyr::mutate(FIPS = paste0(STATEFP,COUNTYFP)) # add unique county identifier

counties$centroid <- sf::st_centroid(counties$geometry)

# more detailed county information for determining logo locations
counties_detailed <- tigris::counties() |>
  dplyr::filter(!STATEFP %in% c(72,78,69,60,66,78)) |> # filtering out territories
  sf::st_transform("+proj=longlat +datum=WGS84") |> # Reproject to WGS84
  dplyr::mutate(FIPS = paste0(STATEFP,COUNTYFP)) # add unique county identifier

counties_detailed$centroid <- sf::st_centroid(counties_detailed$geometry)


# Calculating Distance between every county and school -----------------------
cli::cli_alert_info("Calculating distances...")

comparing_distances <- tidyr::expand_grid(undefeated_subset,
                                          counties |>
                                            dplyr::select(FIPS, centroid) |>
                                            sf::st_drop_geometry())

comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

comparing_distances$distances <- sf::st_distance(comparing_distances$location$x, comparing_distances$centroid$x, by_element = TRUE)

comparing_distances <- comparing_distances |>
  dplyr::select(FIPS,team_id,distances) |>
  dplyr::group_by(FIPS) |>
  dplyr::slice(which.min(distances))


# Determine best colors and logos to use for the map ---------------------------
cli::cli_alert_info("Deciding on best colors and logos...")

styling <- read.csv("C:/Users/lwget/Downloads/styling.csv")

# getting best colors
undefeated_teams <- undefeated_teams |>
  dplyr::mutate(best_color = dplyr::case_when(
    school %in% styling$alt_color_list ~ alt_color,
    !school %in% styling$alt_color_list ~ color
  ))

# getting best logos
undefeated_teams <- undefeated_teams |>
  dplyr::mutate(best_logo = dplyr::case_when(
    school %in% styling$alt_logo_list ~ logo_2,
    !school %in% styling$alt_logo_list ~ logo
  ))

undefeated_teams <- undefeated_teams |> dplyr::arrange(school)

# Create data frame for color mapping -----------------------------------------
cli::cli_alert_info("Creating data frame for color mapping...")

master_df <- comparing_distances |>
  dplyr::select(FIPS,team_id) |>
  dplyr::left_join(counties |>
                     tigris::shift_geometry() |> # shift Alaska and Hawaii below
                     sf::st_transform("+proj=longlat +datum=WGS84") |>
                     dplyr::select(FIPS, NAME, geometry), by = "FIPS", keep = FALSE) |>
  dplyr::left_join(undefeated_teams |>
                     dplyr::select(team_id, school,best_color), by = "team_id", keep = FALSE)

master_df <- sf::st_as_sf(master_df)


# Create data frame for logo mapping -------------------------------------------
cli::cli_alert_info("Creating data frame for logo mapping...")

master_logo_df <- comparing_distances |>
  dplyr::select(FIPS,team_id) |>
  dplyr::left_join(counties_detailed |>
                     tigris::shift_geometry() |>
                     dplyr::filter(!STATEFP %in% c(72,78,69,60,66,78)) |>
                     sf::st_transform("+proj=longlat +datum=WGS84") |>
                     dplyr::select(FIPS, NAME, geometry), by = "FIPS", keep = FALSE) |>
  dplyr::left_join(undefeated_teams |>
                     dplyr::select(team_id, school,best_logo), by = "team_id", keep = FALSE)


master_logo_df <- sf::st_as_sf(master_logo_df)

# getting logo locations --------------------------------------------------
cli::cli_alert_info("Getting logo locations...")

# Apply the function to the example data frame
school_zones <- master_logo_df |>
  dplyr::group_by(school) |>
  dplyr::summarise(geometry = sf::st_union(geometry, is_coverage = TRUE))

school_zones <- sf::st_cast(school_zones |> dplyr::select(school, geometry),
                            "MULTIPOLYGON")

school_zones <- sf::st_cast(school_zones |> dplyr::select(school, geometry),
                            "POLYGON")

school_zones <- sf::st_as_sf(school_zones)

# if there is more than one logo, find area of each polygon
# biggest polygon stays, the rest must be over a certain size to remain
school_zones <- school_zones |>
  dplyr::mutate(area = sf::st_area(school_zones$geometry))

school_zones$area <- as.numeric(school_zones$area)

school_zones <- dplyr::mutate(school_zones,
                              id = dplyr::row_number())

largest_area <- school_zones |>
  dplyr::group_by(school) |>
  dplyr::filter(area == max(area))

filtered <- school_zones |>
  dplyr::group_by(school) |>
  dplyr::filter(area > threshold)

result <- dplyr::bind_rows(largest_area, filtered) |>
  dplyr::distinct(id, .keep_all = TRUE) |>
  dplyr::arrange(school)

logo_location <- result |>
  dplyr::mutate(centroid = sf::st_centroid(result$geometry))

logo_location <- dplyr::left_join(logo_location,
                                  undefeated_teams |>
                                    dplyr::select(school,best_logo),
                                  by = "school")

logo_location <- logo_location |> dplyr::arrange(school)

# logo_copy <- logo_location
#
# logo_copy_2 <- logo_copy |>
#   dplyr::group_by(id) |>
#   dplyr::summarise(geometry = sf::st_nearest_points(centroid, geometry))
#
# logo_copy_2$distance <- sf::st_length(logo_copy_2$geometry)

# Create logos for map --------------------------------------------------------
cli::cli_alert_info("Creating logos for map...")

#icon_size <- (logo_location$area/5e+12)
icon_size <- (as.numeric(log(logo_location$area)) - 21) * 22


logoIcons <- leaflet::icons(
  iconUrl = logo_location$best_logo,
  # magic to adjust the icon size based on territory area
  iconWidth = icon_size,
  iconHeight = icon_size
)

logo_location <- sf::st_drop_geometry(logo_location)
logo_location <- sf::st_as_sf(logo_location)


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
  leaflet::addPolygons(data = master_df,
                       color = "#434445",
                       weight = 0,
                       fillColor = ~best_color,
                       fillOpacity = 0.9,
                       smoothFactor = 0.2,
                       stroke = F,
                       label = ~school) |>
  leaflet::addMarkers(data = logo_location,
                      label = ~school,
                      icon = logoIcons) |>
  leaflet::addPolylines(data = master_df,
                        color = "grey",
                        weight = 0.25,
                        smoothFactor = 0,
                        opacity = 0.75)  |>
  leaflet::addPolylines(data = states,
                        color = "black",
                        weight = 1,
                        smoothFactor = 0,
                        opacity = 1)
m





