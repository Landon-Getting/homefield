#' hf_racing
#' @description
#' Creates a racing bar chart to show map summary statistics over time and
#' saves as a .gif file at a specified location.
#'
#' @param x Data frame created by homefield_stats() or including the following columns:\cr
#' \cr
#' \strong{entity} - identifies each entity (ex. school name - Iowa State,
#' Minnesota, Bowling Green).\cr
#' \cr
#' \strong{color} - hexadecimal color to fill entity bars (ex. #cfab7a).\cr
#' \cr
#' \strong{image} - image url or local file path to be placed on entity bars.\cr
#' \cr
#' \strong{time} - date-times representing when each set of summary statistics
#' was relevant.\cr
#' \cr
#' \strong{\emph{stat_name}} - Summary statistic column with name specified by
#' the stat_name argument. Examples from homefield_stats() include land, water,
#' domain, and population. \cr
#' \cr
#'
#' @param stat_name (String required): Name of the summary statistic column
#' which determines bar size and ordering.
#'
#' @param output_file (String required): Local file path ending in \emph{.gif}.
#'
#' @param title (String required): Title of the map.
#'
#' @param subtitle (String required): Subtitle of the map. Usually indicates
#' label of the summary statistic.
#'
#' @param caption (String required): Caption of the map. Usually includes
#' provides credit to data origins.
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Creating racing bar chart for cfb conquest maps in 2021
#'
#' # undefeated for each week, 0 through 15
#' x_input <- list(cfb_conquest(season = 2021, week = 0),
#'                cfb_conquest(season = 2021, week = 1),
#'                cfb_conquest(season = 2021, week = 2),
#'                cfb_conquest(season = 2021, week = 3),
#'                cfb_conquest(season = 2021, week = 4),
#'                cfb_conquest(season = 2021, week = 5),
#'                cfb_conquest(season = 2021, week = 6),
#'                cfb_conquest(season = 2021, week = 7),
#'                cfb_conquest(season = 2021, week = 8),
#'                cfb_conquest(season = 2021, week = 9),
#'                cfb_conquest(season = 2021, week = 10),
#'                cfb_conquest(season = 2021, week = 11),
#'                cfb_conquest(season = 2021, week = 12),
#'                cfb_conquest(season = 2021, week = 13),
#'                cfb_conquest(season = 2021, week = 14),
#'                cfb_conquest(season = 2021, week = 15))
#'
#'# time in weeks, 0 through 15
#'temporal_input <- lubridate::ymd(c("2021-08-28",
#'                                   "2021-09-04",
#'                                   "2021-09-11",
#'                                   "2021-09-18",
#'                                   "2021-09-25",
#'                                   "2021-10-2",
#'                                   "2021-10-9",
#'                                   "2021-10-16",
#'                                   "2021-10-23",
#'                                   "2021-10-30",
#'                                   "2021-11-6",
#'                                   "2021-11-13",
#'                                   "2021-11-20",
#'                                   "2021-11-26",
#'                                   "2021-12-4",
#'                                  "2021-12-11"))
#'
#'
#'temporal_stats <- homefield_stats(x = x_input,
#'                                temporal = temporal_input,
#'                                keep_max = FALSE,
#'                                keep_visuals = TRUE)
#'
#'# converting land from square meters to square miles
#'temporal_stats_plot <- temporal_stats |>
#'  dplyr::mutate(land = land/2.59e6)
#'
#'hf_racing(x = temporal_stats_plot,
#'                stat_name = "land",
#'                title = "2021 Season Week by Week - CFB Conquest homefield Map",
#'                subtitle = "Area in Square Miles",
#'                caption = "Data Source: cfbd.com",
#'                output_file = "C:/Users/darthvader/Downloads/cfb_conquest_2021_racing.gif")
#' }
hf_racing <- function(x,
                            stat_name,
                            output_file,
                            title = "Racing Bar Chart",
                            subtitle = "Value Label",
                            caption = "Data Source"){

  cli::cli_h1("Generating homefield racing bar graph...")

  cli::cli_alert_info("Checking inputs...")

  # INPUT CHECKS --------------------------------------------------------------
  # setting expected column names
  expected_cols <- c("entity",
                     "color",
                     "image",
                     "time",
                     stat_name)

  if(!is.character(stat_name)){
    stop("stat_name must be of type character.")
  }

  # checking if column names match expected names
  if (!all(expected_cols %in% names(x))) {
    stop("x must contain columns entity, color, image, time, and the specified stat_name.")
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

  # if starts with http, check if valid URL
  if(all(sapply(x$image, function(x) substr(x, 1, 4) == "http"))) {
    if(!(all(sapply(x$image, valid_url)))) {
      stop("x must contain an image column with valid urls or accessible local file paths.")
    }
  }

  # checking image column for valid url or file paths
  if((all(dir.exists(x$image)) & all(file.access(x$image, mode = 4)))) {
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

  cli::cli_alert_info("Preparing data...")
  x_ready <- x |>
    dplyr::group_by(.data$time) |>
    dplyr::mutate(rank = rank(-get(stat_name)),
                  entity_rel = get(stat_name)/get(stat_name)[rank==1],
                  entity_lbl = paste0(" ",round(get(stat_name)))) |>
    dplyr::group_by(.data$entity) |>
    dplyr::filter(rank <=10) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$time, rank)|>
    dplyr::mutate(entity_lbl = prettyNum(.data$entity_lbl,
                                             big.mark = ","))

  cli::cli_alert_info("Creating static plot...")

  # Creating Static Plot-------------------------------------------------------
  static_plot <- ggplot2::ggplot(x_ready,
                                 ggplot2::aes(group = .data$entity)) +
    ggpattern::geom_rect_pattern(
      ggplot2::aes(pattern_filename = I(.data$image),
                   fill = .data$color,
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
                                    label = paste(.data$entity, " ")),
                       vjust = 0.2,
                       hjust = 1,
                       size = 10.5) +
    ggplot2::geom_text(ggplot2::aes(x = rank,
                                    y= get(stat_name),
                                    label = .data$entity_lbl,
                                    hjust=0),
                       size = 10) +
    ggplot2::geom_text(ggplot2::aes(x=10,
                                    y=max(get(stat_name)),
                                    label = as.factor(.data$time)),
                       vjust = 0.2,
                       alpha = 0.5,
                       col = "gray",
                       size = 20) +
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
                   plot.margin = ggplot2::margin(2, 6, 2, 10, "cm")) +
                    ggimage::geom_image(ggplot2::aes(image = "./inst/figures/sticker.png",
                                                    x = 8,
                                                    y = max(get(stat_name))),
                                         size = 0.2)

  cli::cli_alert_info("Animating...")
  # Animating -----------------------------------------------------------------
  animated <- static_plot + gganimate::transition_states(.data$time,
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

  cli::cli_alert_info("Rendering...")
  # Rendering ------------------------------------------------------------------
  gganimate::animate(animated,
                     nframes = 300,
                     fps = 10,
                     end_pause = 30,
                     width = 1500,
                     height = 1000,
                     renderer = gganimate::gifski_renderer(output_file))

  cli::cli_alert_info("Done!")

}
