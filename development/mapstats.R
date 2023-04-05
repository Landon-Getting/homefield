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
