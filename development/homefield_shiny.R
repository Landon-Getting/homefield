library(shiny)

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

# Reprojection
epsg2163 <- leaflet::leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "ESRI:102003",
  proj4def = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  resolutions = 2^(16:7)
)

# ui object
ui <- fluidPage(
  titlePanel(p("homefield - Interactive CFB Map", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      helpText(
        "Welcome to the homefield Shiny App!
        There are two college football datasets to visualize.
        In `Undefeated`, unbeaten teams control the counties closest to them.
        In `Conquest`, teams gain another team's territory when they defeat them."
      ),
      selectInput(
        inputId = "mapType",
        label = "Select a type of map",
        choices = c("Undefeated",
                    'Conquest'),
        selected = "Undefeated"
      ),
      selectInput(
        inputId = "season",
        label = "Select a season",
        choices = sort(1950:2022, decreasing = TRUE),
        selected = 2022
      ),
      selectInput(
        inputId = "week",
        label = "Select a week",
        choices = 0:14,
        selected = 0
      ),
      actionButton("generate_map", "Generate Map"),
    ),
    mainPanel(
      leaflet::leafletOutput(outputId = "map",
                             height = 600),
    )
  )
)


server <- function(input, output) {

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
        dplyr::select(.data$entity,
                      .data$lat,
                      .data$lng)

      setProgress(0.4, detail = "Calculating distances...")
      # converting lat/long to sf point object -------------------------------------
      input_df_location <-  sf::st_as_sf(input_df_location,
                                         coords = c("lng", "lat"),
                                         crs = 4326) |>
        dplyr::rename(location = .data$geometry)


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
                      .data$entity,
                      .data$distances) |>
        dplyr::group_by(.data$GEOID) |>
        dplyr::slice(which.min(.data$distances))

      setProgress(0.6, detail = "Coloring map...")
      # Create data frame for color mapping -----------------------------------------

      homefield_map_df <- comparing_distances |>
        dplyr::select(.data$GEOID,
                      .data$entity) |>
        dplyr::left_join(counties |>
                           tigris::shift_geometry() |> # shift Alaska and Hawaii below
                           sf::st_transform("+proj=longlat +datum=WGS84") |>
                           dplyr::select(.data$GEOID,
                                         .data$geometry),
                         by = "GEOID",
                         keep = FALSE) |>
        dplyr::left_join(input_df |>
                           dplyr::select(.data$entity,
                                         .data$color,
                                         .data$image)|>
                           dplyr::distinct(),
                         by = "entity",
                         keep = FALSE)

      homefield_map_df <- sf::st_as_sf(homefield_map_df) |>
        dplyr::mutate(border_color = contrast_color(.data$color))

      setProgress(0.8, detail = "Adding images to the map...")
      # Apply the function to the example data frame
      counties_grouped <- homefield_map_df |>
        dplyr::group_by(.data$entity) |>
        dplyr::summarise(geometry = suppressWarnings(sf::st_union(.data$geometry,
                                                                  is_coverage = TRUE))) # reduces calculation time

      counties_grouped <- sf::st_cast(counties_grouped |>
                                        dplyr::select(.data$entity,
                                                      .data$geometry),
                                      "MULTIPOLYGON")

      counties_grouped <- sf::st_cast(counties_grouped |>
                                        dplyr::select(.data$entity,
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
        dplyr::group_by(.data$entity) |>
        dplyr::filter(.data$area == max(.data$area))

      above_threshold_areas <- counties_grouped |>
        dplyr::group_by(.data$entity) |>
        dplyr::filter(.data$area > 1e10)

      image_areas <- dplyr::bind_rows(largest_areas, above_threshold_areas) |>
        dplyr::distinct(.data$id, .keep_all = TRUE) |>
        dplyr::arrange(.data$entity) |>
        dplyr::mutate(centroid = sf::st_centroid(.data$geometry)) |>
        dplyr::left_join(input_df |> dplyr::select(.data$entity,
                                                   .data$image),
                         by = "entity") |>
        dplyr::arrange(.data$entity)


      # magic to adjust the icon size based on territory area
      icon_size <- (as.numeric((image_areas$area/1e11)) + 1) * 20

      icon_size <- ifelse(icon_size > 80, 80, icon_size)

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
                                                            zoomSnap = 0.1,
                                                            zoomDelta = 1),
                          height = 2000,
                          width = 3200) |>
      leaflet::setView(lng = -100.64580,
                       lat = 38.05909,
                       zoom = 3.3) |>
      leaflet::addPolylines(data = states,
                            color = "black",
                            weight = 1,
                            smoothFactor = 0,
                            opacity = 1)
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
                          label = ~entity,
                          icon = logoIcons,
                          group = "changing")

  })

}

shinyApp(ui = ui, server = server)
