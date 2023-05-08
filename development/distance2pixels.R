library(sf)
library(leaflet)

# loading spatial data
spatial_data <- st_read(system.file("shape/nc.shp", package="sf")) |>
  sf::st_transform("+proj=longlat +datum=WGS84")

# gathering a single polygon as an example from spatial data
example_polygon <- spatial_data[1,15]

#  polygon centroid point
polygon_centroid <- sf::st_centroid(example_polygon)

# distance from polygon centroid to closest point on polygon (as linestring)
desired_distance_linestring <- sf::st_nearest_points(example_polygon,
                                             polygon_centroid,
                                             pairwise = TRUE)

# returns desired distance in meters rather than as a linestring
# NEED PIXELS RATHER THAN METERS
#desired_distance <- sf::st_length(desired_distance_linestring)

# Creating leaflet map
leaflet::leaflet() |>
  # plotting example polygon
  leaflet::addPolygons(data = example_polygon,
                       fillOpacity = 0,
                       color = "red",
                       weight = 1) |>
  # visualizing desired distance
  leaflet::addPolygons(data = desired_distance_linestring,
                       fillOpacity = 0,
                       color = "blue",
                       weight = 1)





