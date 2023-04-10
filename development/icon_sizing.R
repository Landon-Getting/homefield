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
