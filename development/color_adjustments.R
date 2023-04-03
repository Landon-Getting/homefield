x

territorymap(x,
             output_file = "C:/Users/lwget/Downloads/territorymap.png",
             title = "Closest Undefeated D1 Football Team to each US County \n Season 2021 Week 6",
             credit = "Landon Getting")

get_fg_color <- function(bg_color) {

  # Convert the background color to RGB
  bg_rgb <- as.integer(strsplit(bg_color, "")[[1]][2:7])
  bg_rgb <- matrix(bg_rgb, ncol = 3, byrow = TRUE) / 255

  # Calculate the luminance of the background color
  lum <- 0.2126 * bg_rgb[1] + 0.7152 * bg_rgb[2] + 0.0722 * bg_rgb[3]

  # Calculate the desired luminance of the foreground color
  desired_lum <- ifelse(lum > 0.5, 0, 1)

  # Scale the foreground color based on the desired and actual luminance
  fg_rgb <- desired_lum * rep(1, 3) + (1 - desired_lum) * rep(0, 3)
  fg_rgb <- fg_rgb + (1 - lum) * (bg_rgb - fg_rgb)
  fg_rgb <- round(fg_rgb * 255)

  # Convert the foreground color back to a hexadecimal string
  fg_color <- paste0("#", paste(sprintf("%02X", fg_rgb), collapse = ""))

  return(fg_color)
}
