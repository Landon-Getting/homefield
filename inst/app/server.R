function(input, output) {

  image <- entity <- lat <- lng <- geometry <- STUSPS <- GEOID <- centroid <- NULL
  distances <- color <- image <- area <- id <- NULL

  reactive_output <- eventReactive(input$generate_map, {

    withProgress(message = 'homefield', value = 0, {

      setProgress(0.2, detail = "Querying data...")

      if(input$mapType == "Undefeated"){
        data_working <- homefield::cfb_undefeated(season = input$season, week = input$week)

      } else {
        data_working <- homefield::cfb_conquest(season = input$season, week = input$week)
      }

      input_df <- data_working

      input_df_location <- input_df |>
        dplyr::select(entity,
                      lat,
                      lng)

      setProgress(0.4, detail = "Calculating distances...")
      # converting lat/long to sf point object -------------------------------------
      input_df_location <-  sf::st_as_sf(input_df_location,
                                         coords = c("lng", "lat"),
                                         crs = 4326) |>
        dplyr::rename(location = geometry)


      comparing_distances <- tidyr::expand_grid(input_df_location,
                                                counties |>
                                                  dplyr::select(GEOID, centroid) |>
                                                  sf::st_drop_geometry())

      comparing_distances$location <- sf::st_as_sf(comparing_distances$location)
      comparing_distances$centroid <- sf::st_as_sf(comparing_distances$centroid)

      comparing_distances$distances <- sf::st_distance(comparing_distances$location$x,
                                                       comparing_distances$centroid$x,
                                                       by_element = TRUE)

      comparing_distances <- comparing_distances |>
        dplyr::select(GEOID,
                      entity,
                      distances) |>
        dplyr::group_by(GEOID) |>
        dplyr::slice(which.min(distances))

      setProgress(0.6, detail = "Mixing colors...")
      # Create data frame for color mapping -----------------------------------------

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
        dplyr::left_join(input_df |>
                           dplyr::select(entity,
                                         color,
                                         image)|>
                           dplyr::distinct(),
                         by = "entity",
                         keep = FALSE)

      homefield_map_df <- sf::st_as_sf(homefield_map_df) |>
        dplyr::mutate(border_color = contrast_color(color))

      setProgress(0.8, detail = "Arranging images...")
      # Apply the function to the example data frame
      counties_grouped <- homefield_map_df |>
        dplyr::group_by(entity) |>
        dplyr::summarise(geometry = suppressWarnings(sf::st_union(geometry,
                                                                  is_coverage = TRUE))) # reduces calculation time

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

      # if there is more than one logo, find area of each polygon
      # biggest polygon stays, the rest must be over a certain size to remain
      counties_grouped <- counties_grouped |>
        dplyr::mutate(area = as.numeric(sf::st_area(counties_grouped$geometry)),
                      id = dplyr::row_number())

      largest_areas <- counties_grouped |>
        dplyr::group_by(entity) |>
        dplyr::filter(area == max(area))

      above_threshold_areas <- counties_grouped |>
        dplyr::group_by(entity) |>
        dplyr::filter(area > 1e10)

      image_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
        dplyr::distinct(id, .keep_all = TRUE) |>
        dplyr::arrange(entity) |>
        dplyr::mutate(centroid = sf::st_centroid(geometry)) |>
        dplyr::left_join(input_df |> dplyr::select(entity,
                                                   image),
                         by = "entity") |>
        dplyr::arrange(entity)


      # magic to adjust the icon size based on territory area
      # distance from polygon centroid to closest point on polygon (as linestring)
      best_center <- sf::st_as_sf(geomander::st_circle_center(image_areas)) |>
        sf::st_transform("+proj=longlat +datum=WGS84")

      image_areas$best_center <- best_center$geometry

      image_areas$distance <- sf::st_length(sf::st_nearest_points(image_areas$geometry,
                                                                  image_areas$best_center,
                                                                  pairwise = TRUE))

      image_areas$best_center_lng <- sf::st_coordinates(image_areas$best_center)[,1]
      image_areas$best_center_lat <- sf::st_coordinates(image_areas$best_center)[,2]


      # magic to adjust the icon size based on territory area
      icon_size <- as.numeric(image_areas$distance/3000)

      logoIcons <- leaflet::icons(
        iconUrl = image_areas$image,
        iconWidth = icon_size,
        iconHeight = icon_size
      )

      image_areas <- sf::st_drop_geometry(image_areas)
      image_areas <- sf::st_as_sf(image_areas)

      reactive_output <- list(homefield_map_df = homefield_map_df,
                              logoIcons = logoIcons,
                              image_areas = image_areas)

      return(reactive_output)
      setProgress(1, detail = "Nearly ready...")
    })

  })

  output$map <- leaflet::renderLeaflet({

    leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                                       zoomControl = FALSE,
                                                       minZoom = 3.3,
                                                       maxZoom = 3.3,
                                                       dragging = FALSE),
                     height = 2000,
                     width = 3200) |>
      leaflet::setView(lng = -100.64580,
                       lat = 37.05909,
                       zoom = 3.3) |>
      leaflet::addPolylines(data = states,
                            color = "black",
                            weight = 1,
                            smoothFactor = 0,
                            opacity = 1) |>
      leaflet.extras::setMapWidgetStyle(list(background= "white"))
  })

  # Update territories
  observe({

    homefield_map_df <- reactive_output()$homefield_map_df
    logoIcons <- reactive_output()$logoIcons
    image_areas <- reactive_output()$image_areas

    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("changing") |>
      leaflet::addPolygons(data = homefield_map_df,
                           fillColor = ~color,
                           fillOpacity = 0.9,
                           smoothFactor = 0.2,
                           color = ~border_color,
                           weight = 0.25,
                           opacity = 0.75,
                           stroke = T,
                           label = ~entity,
                           group = "changing") |>
      leaflet::addMarkers(data = image_areas,
                          lat = image_areas$best_center_lat,
                          lng = image_areas$best_center_lng,
                          label = ~entity,
                          icon = logoIcons,
                          group = "changing")

  })

}
