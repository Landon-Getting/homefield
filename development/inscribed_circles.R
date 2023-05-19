library(sf)
library(leaflet)

# loading spatial data
spatial_data <- st_read(system.file("shape/nc.shp", package="sf")) |>
  sf::st_transform("+proj=longlat +datum=WGS84")

best_centers <-  sf::st_as_sf(geomander::st_circle_center(spatial_data)) |>
  sf::st_transform("+proj=longlat +datum=WGS84")

best_centers_distance <- sf::st_length(sf::st_nearest_points(spatial_data,
                                               best_centers,
                                               pairwise = TRUE))

circles <- sf::st_buffer(best_centers, best_centers_distance)

# Creating leaflet map
leaflet::leaflet() |>
  # plotting example polygon
  leaflet::addPolygons(data = spatial_data,
                       fillOpacity = 0,
                       color = "red",
                       weight = 1) |>
  # visualizing desired distance
  leaflet::addPolygons(data = circles,
                       fillOpacity = 0,
                       color = "blue",
                       weight = 1)





