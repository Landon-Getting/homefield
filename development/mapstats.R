# objective
# get temporal map stats
# use case - get map stats for each week of imperialism up to week 6


week = 4
year = 2022

for(week_number in 0:week){

  if(week_number == 0){

    temporal_stats <- get_map_stats(territorymap::get_cfb_imperialism(season = year, week_number))|>
      dplyr::mutate(week = week_number)

  } else{

    x <- get_map_stats(territorymap::get_cfb_imperialism(season = year, week_number)) |>
      dplyr::mutate(week = week_number)

    temporal_stats <- rbind(temporal_stats, x)
  }

}

View(temporal_stats)

top_10 <- temporal_stats |>
  dplyr::filter(week == 6) |>
  dplyr::top_n(10, land) |>
  dplyr::select(identifier)

plot_stats <- temporal_stats |>
  dplyr::filter(identifier %in% top_10$identifier)

ggplot2::ggplot(data = plot_stats, ggplot2::aes(x = week, y = pop, color = identifier)) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "Week", y = "Population") +
  ggplot2::theme_bw()


# temporal example
x_input <- list(get_cfb_undefeated(season = 2021, week = 0),
                 get_cfb_undefeated(season = 2021, week = 1),
                 get_cfb_undefeated(season = 2021, week = 2),
                 get_cfb_undefeated(season = 2021, week = 3),
                 get_cfb_undefeated(season = 2021, week = 4))

# week 0 through 4
temporal_input <- c(0,
                    1,
                    2,
                    3,
                    4)

temporal_stats <- territorymap::get_map_stats(x = x_input, temporal = temporal_input)











