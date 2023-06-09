STUSPS <- NULL

states <- get0("states", envir = asNamespace("homefield")) |>
  tigris::shift_geometry() |>
  sf::st_transform("+proj=longlat +datum=WGS84") # Reproject to WGS84

counties <- get0("counties", envir = asNamespace("homefield")) |>
  sf::st_transform("+proj=longlat +datum=WGS84") # Reproject to WGS84

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
