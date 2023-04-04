x

territorymap(x,
             output_file = "C:/Users/lwget/Downloads/territorymap.png",
             title = "Closest Undefeated D1 Football Team to each US County \n Season 2021 Week 6",
             credit = "Landon Getting")



contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  r <- (rgb.array[1] ^ 2) * .068
  g <- (rgb.array[2] ^ 2) * .691
  b <- (rgb.array[3] ^ 2) * .241

  brightness <- 255 - sqrt(r + g + b)

  if(brightness > 80 & brightness < 128){
    brightness <- 80
  } else if(brightness > 128 & brightness < 240){
    brightness <- 240
  }

  if(brightness > 128){
    brightness <- 0
  } else{
    brightness <- 255
  }


  hex_output <- rgb(brightness, brightness, brightness, maxColorValue=255)

  return(hex_output)
}

hex_input <-	"#00274c"
brightness


contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  r <- (rgb.array[1] ^ 2) * 0.299
  g <- (rgb.array[2] ^ 2) * 0.587
  b <- (rgb.array[3] ^ 2) * 0.114

  brightness <- 255 - sqrt(r + g + b)

  hex_output <- rgb(brightness, brightness, brightness, maxColorValue=255)

  return(hex_output)
}

contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  r <- rgb.array[1] * 299
  g <- rgb.array[2] * 587
  b <- rgb.array[3] * 114

  brightness <- (r + g + b)/1000

  hex_output <- rgb(brightness, brightness, brightness, maxColorValue=255)

  return(hex_output)
}


contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  r <- (rgb.array[1] * 0.2126)
  g <- (rgb.array[2] * 0.7152)
  b <- (rgb.array[3] * 0.0722)

  brightness <- (r + g + b)

  hex_output <- rgb(brightness, brightness, brightness, maxColorValue=255)

  return(hex_output)
}


contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  r <- (rgb.array[1] ^ 2) * 0.2126
  g <- (rgb.array[2] ^ 2) * 0.7152
  b <- (rgb.array[3] ^ 2) * 0.0722

  brightness <- 255 - sqrt(r + g + b)

  hex_output <- rgb(brightness, brightness, brightness, maxColorValue=255)

  return(hex_output)
}


contrast_color <- function(hex_input) {

  rgb.array <- col2rgb(hex_input)
  brightness <- (rgb.array[1] * 299 + rgb.array[2] * 587 + rgb.array[3] * 114) / 1000
  brightness_scale <- ifelse(brightness < 128, 1.5, 0.5)
  brightness <- brightness * brightness_scale

  hex_output <- rgb(brightness, brightness, brightness, maxColorValue = 255)
  return(hex_output)
}

brightness
hex_input = "#ecdcb9"
hex_output

contrast_color('#d12c21')

x <- get_cfb_undefeated(season = 2021, week = 2)

territorymap(x,
             output_file = "C:/Users/lwget/Downloads/territorymap.png",
             title = "Closest Undefeated D1 Football Team to each US County \n Season 2021 Week 6",
             credit = "Landon Getting")

output_file = "C:/Users/lwget/Downloads/territorymap.png"
title = "Closest Undefeated D1 Football Team to each US County \n Season 2021 Week 6"
credit = "Landon Getting"


