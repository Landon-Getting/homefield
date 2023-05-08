poly_test <- counties_grouped[1,] |> dplyr::select(geometry)

library(sf)

inscribed_circle <- function(poly) {
  # Get centroid of polygon
  centroid <- sf::st_centroid(poly)

  # Get nearest point
  radius <- sf::st_length(sf::st_nearest_points(poly, centroid))

  # Create circle as a point with a buffer around it
  circle <- sf::st_buffer(centroid, radius)

  return(circle)
}

inscribed <- inscribed_circle(poly_test)

library(sf)
library(ggplot2)

ggplot() +
  geom_sf(data = poly_test, fill = "transparent", color = "red") +
  geom_sf(data = inscribed, fill = "transparent", color = "blue") +
  coord_sf() +
  theme_void()


image_areas <- dplyr::mutate(image_areas,
                             territory_border = sf::st_length(sf::st_nearest_points(image_areas$geometry,
                                                                                    image_areas$centroid,
                                                                                    pairwise = TRUE)))

icon_size <- image_areas$territory_border/1400



# More testing ----------------------------------------------------------------

library(sf)
library(leaflet)

# create a simple sf polygon
polys <- st_polygon(list(list(cbind(c(0,0,1,1,0), c(0,1,1,0,0)))))
polys_sf <- st_sf(id=1, geometry=polys)

# create a leaflet map
m <- leaflet() %>%
  setView(lng=0.5, lat=0.5, zoom=3) %>%
  addTiles()

# add the polygon to the map
m <- m %>% addPolygons(data=polys_sf)

# get the size of the polygon in pixels
poly_layer_id <- paste0("leaflet-", m$mapId, "-shapes-", polys_sf$id)
poly_size <- htmlwidgets::JS(sprintf("function(){return this.getSize().y;}")) %>%
  leaflet::JS() %>%
  leaflet::evaluate(m, poly_layer_id)

# print the size of the polygon in pixels
cat("The polygon size in pixels is:", poly_size, "\n")























