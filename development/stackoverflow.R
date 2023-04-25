#install.packages("leaflet")
#install.packages("sf")

library(leaflet)
library(sf)

# load data from sf package
nc <- st_read(system.file("shape/nc.shp", package="sf")) |>
  sf::st_transform("+proj=longlat +datum=WGS84")

# subsetting for smaller map
nc <- nc[1:3,]

# markers will be placed at the centroid of the polygon
nc$centroid <- sf::st_centroid(nc$geometry)

# generating markers
icons <- leaflet::icons(
  iconUrl = c("https://cdn-icons-png.flaticon.com/512/985/985331.png",
              "https://cdn-icons-png.flaticon.com/512/985/985382.png",
              "https://cdn-icons-png.flaticon.com/512/985/985280.png"),
  iconWidth = 40,
  iconHeight = 40
)

# Creating leaflet map with polygons and markers
leaflet() |>
  addPolygons(data = nc,
              fillOpacity = 1,
              fillColor = "white",
              color = "black",
              weight = 1) |>
  leaflet::addMarkers(data = nc$centroid,
                      icon = icons)

