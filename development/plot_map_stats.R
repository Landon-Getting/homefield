# undefeated for each week, 0 through 15
x_input <- list(get_cfb_imperialism(season = 2021, week = 0),
                get_cfb_imperialism(season = 2021, week = 1),
                get_cfb_imperialism(season = 2021, week = 2),
                get_cfb_imperialism(season = 2021, week = 3),
                get_cfb_imperialism(season = 2021, week = 4),
                get_cfb_imperialism(season = 2021, week = 5),
                get_cfb_imperialism(season = 2021, week = 6),
                get_cfb_imperialism(season = 2021, week = 7),
                get_cfb_imperialism(season = 2021, week = 8),
                get_cfb_imperialism(season = 2021, week = 9),
                get_cfb_imperialism(season = 2021, week = 10),
                get_cfb_imperialism(season = 2021, week = 11),
                get_cfb_imperialism(season = 2021, week = 12),
                get_cfb_imperialism(season = 2021, week = 13),
                get_cfb_imperialism(season = 2021, week = 14),
                get_cfb_imperialism(season = 2021, week = 15))

# time in weeks, 0 through 15
temporal_input <- c(0:15)

temporal_stats <- get_map_stats(x = x_input,
                                temporal = temporal_input,
                                keep_max = FALSE,
                                keep_visuals = TRUE)


temporal_stats_plot <- temporal_stats |>
  dplyr::mutate(land = land/1e6)

stat_name_input = "land"
title_input = "2021 Season Week by Week - Imperialism CFB Territory Map"
subtitle_input = "Area in Square Kilometers"
caption_input = "Data Source: cfbd.com"
output_file_input = "C:/Users/lwget/Downloads/racing_imperialism.gif"

territoryracing(x = temporal_stats_plot,
                 stat_name = stat_name_input,
                 title = title_input,
                 subtitle = subtitle_input,
                 caption = caption_input,
                 output_file = output_file_input)


# NEW SOLUTION FROM SCRATCH W GGANIMATE
# inspired by https://rpubs.com/haginta/709479

territoryracing <- function(x,
                             stat_name,
                             output_file,
                             title = "Racing Bar Chart",
                             subtitle = "Value Label",
                             caption = "Data Source"){

  # INPUT CHECKS --------------------------------------------------------------
  # setting expected column names
  expected_cols <- c("identifier",
                     "color",
                     "image",
                     "time",
                     stat_name)

  if(!is.character(stat_name)){
    stop("stat_name must be of type character.")
  }

  # checking if column names match expected names
  if (!all(expected_cols %in% names(x))) {
    stop("x must contain columns identifier, color, image, time, and the specified stat_name.")
  }

  # getting column sizes
  col_lengths <- sapply(x, length)

  # Checking if all column lengths are equal
  if (!length(unique(col_lengths)) == 1) {
    stop("x must contain columns of equal size.")
  }

  # Defining a regular expression for matching valid hex color codes
  hex_regex <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

  # Checking if the color column contains valid hex color codes
  if (!all(grepl(hex_regex, x$color))) {
    stop("x must contain a color column with valid colors in hexadecimal format.")
  }

  # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
  # creating url checker for image column
  valid_url <- function(url_in,t=2){
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
    suppressWarnings(try(close.connection(con),silent=T))
    ifelse(is.null(check),TRUE,FALSE)
  }

  # checking image column for valid url or file paths
  if(!(all(sapply(x$image,valid_url)) | (all(dir.exists(x$image)) & all(file.access(x$image, mode = 4))))) {
    stop("x must contain an image column with valid urls or accessible local file paths.")
  }

  # checking if output_file path is valid and user has write permission
  if (!dir.exists(dirname(output_file)) | file.access(dirname(output_file), mode = 2)) {
    stop("output_file must be a valid directory path with write access.")
  }

  # Checking if output_file basename is valid
  if (!grepl("^[^[:cntrl:]/?*:;{}\\\\]+\\.[^[:cntrl:]/?*:;{}\\\\]+$", basename(output_file))) {
    stop("Invalid output_file name.")
  }

  # Checking if title is valid
  if(!missing(title)){
    if (!is.character(title)) {
      stop("title must be of type character.")
    }
  }

  # Checking if subtitle is valid
  if(!missing(subtitle)){
    if (!is.character(subtitle)) {
      stop("subtitle must be of type character.")
    }
  }

  # Checking if caption is valid
  if(!missing(caption)){
    if (!is.character(caption)) {
      stop("caption must be of type character.")
    }
  }

  # Preparing Data Frame ------------------------------------------------------

  x_ready <- x |>
    dplyr::group_by(time) |>
    dplyr::mutate(rank = rank(-get(stat_name)),
                  identifier_rel = get(stat_name)/get(stat_name)[rank==1],
                  identifier_lbl = paste0(" ",round(get(stat_name)))) |>
    dplyr::group_by(identifier) |>
    dplyr::filter(rank <=10) |>
    dplyr::ungroup() |>
    dplyr::arrange(time, rank)|>
    dplyr::mutate(identifier_lbl = prettyNum(identifier_lbl,
                                             big.mark = ","))


  # Creating Static Plot-------------------------------------------------------
  static_plot <- ggplot2::ggplot(x_ready,
                                 ggplot2::aes(group = identifier)) +
    ggpattern::geom_rect_pattern(
      ggplot2::aes(pattern_filename = I(image),
                   fill = color,
                   ymin = 0,
                   ymax = get(stat_name),
                   xmin = rank - 0.45,
                   xmax = rank + 0.45),
      pattern         = "image",
      pattern_type    = 'none',
      pattern_scale   = -2,
      pattern_gravity = 'east') +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(ggplot2::aes(x = rank,
                                    y = 0,
                                    label = paste(identifier, " ")),
                       vjust = 0.2,
                       hjust = 1,
                       size = 11) +
    ggplot2::geom_text(ggplot2::aes(x = rank,
                                    y= get(stat_name),
                                    label = identifier_lbl,
                                    hjust=0),
                       size = 10) +
    ggplot2::geom_text(ggplot2::aes(x=10,
                                    y=max(get(stat_name)),
                                    label = as.factor(time)),
                       vjust = 0.2,
                       alpha = 0.5,
                       col = "gray",
                       size = 25) +
    ggplot2::coord_flip(clip = "off",
                        expand = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_reverse() +
    ggplot2::theme(axis.line= ggplot2::element_blank(),
                   axis.text.x= ggplot2::element_blank(),
                   axis.text.y= ggplot2::element_blank(),
                   axis.ticks= ggplot2::element_blank(),
                   axis.title.x= ggplot2::element_blank(),
                   axis.title.y= ggplot2::element_blank(),
                   panel.background= ggplot2::element_blank(),
                   panel.border= ggplot2::element_blank(),
                   panel.grid.major= ggplot2::element_blank(),
                   panel.grid.minor= ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_line(linewidth=.1,
                                                              color="grey" ),
                   panel.grid.minor.x = ggplot2::element_line(linewidth=.1,
                                                              color="grey" ),
                   plot.title = ggplot2::element_text(size=40,
                                                      hjust=0,
                                                      face="bold",
                                                      colour="black",
                                                      vjust=1),
                   plot.subtitle = ggplot2::element_text(size=30,
                                                         vjust = 1,
                                                         hjust= 0,
                                                         face="italic",
                                                         color="grey"),
                   plot.caption = ggplot2::element_text(size=20,
                                                        hjust=1,
                                                        face="italic",
                                                        color="grey"),
                   plot.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::margin(2, 6, 2, 10, "cm"))


  # Animating -----------------------------------------------------------------
  animated <- static_plot + gganimate::transition_states(time,
                                                         transition_length = 8,
                                                         state_length = 2,
                                                         wrap = FALSE) +
    gganimate::view_follow(fixed_x = TRUE)  +
    gganimate::ease_aes('linear')+
    gganimate::enter_fade()+
    gganimate::exit_fade() +
    ggplot2::labs(title = title,
                  subtitle  =  subtitle,
                  caption  = caption)

  # Rendering ------------------------------------------------------------------
  gganimate::animate(animated,
                     nframes = 600,
                     fps = 20,
                     end_pause = 30,
                     width = 1500,
                     height = 1000,
                     renderer = gganimate::gifski_renderer(output_file))


}








































