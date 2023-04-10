library(territorymap)

cfb_undefeated_s2022_w6 <- get_cfb_undefeated(season = 2022, week = 6)

territorymap(x = cfb_undefeated_s2022_w6,
             output_file = "C:/Users/lwget/Downloads/cfb_undefeated_s2022_w6.png",
             title = "College Football Undefeated - Season 2022 Week 6",
             credit = "Landon Getting")


for(i in 0:15){
  territorymap(x = get_cfb_undefeated(season = 2022, week = i),
               output_file = paste0("C:/Users/lwget/Downloads/cfb_undefeated_s2022_w", i,".png"),
               title = paste0("College Football Undefeated - Season 2022 Week ", i),
               credit = "Landon Getting")
}

cfb_undefeated_s2022_w6_stats <- get_map_stats(x = cfb_undefeated_s2022_w6)

View(cfb_undefeated_s2022_w6_stats)

# what was the most land acquired by a team?
max(cfb_undefeated_s2022_w6_stats$land)


# let's look at land over time for the 2022 season

# undefeated for each week, 0 through 15
x_input <- list(get_cfb_undefeated(season = 2022, week = 0),
                get_cfb_undefeated(season = 2022, week = 1),
                get_cfb_undefeated(season = 2022, week = 2),
                get_cfb_undefeated(season = 2022, week = 3),
                get_cfb_undefeated(season = 2022, week = 4),
                get_cfb_undefeated(season = 2022, week = 5),
                get_cfb_undefeated(season = 2022, week = 6),
                get_cfb_undefeated(season = 2022, week = 7),
                get_cfb_undefeated(season = 2022, week = 8),
                get_cfb_undefeated(season = 2022, week = 9),
                get_cfb_undefeated(season = 2022, week = 10),
                get_cfb_undefeated(season = 2022, week = 11),
                get_cfb_undefeated(season = 2022, week = 12),
                get_cfb_undefeated(season = 2022, week = 13),
                get_cfb_undefeated(season = 2022, week = 14),
                get_cfb_undefeated(season = 2022, week = 15))

# time in weeks, 0 through 15
temporal_input <- lubridate::ymd(c("2022-08-27",
                                   "2022-09-03",
                                   "2022-09-10",
                                   "2022-09-17",
                                   "2022-09-24",
                                   "2022-10-1",
                                   "2022-10-8",
                                   "2022-10-15",
                                   "2022-10-22",
                                   "2022-10-29",
                                   "2022-11-5",
                                   "2022-11-12",
                                   "2022-11-19",
                                   "2022-11-26",
                                   "2022-12-3",
                                   "2022-12-10"))

temporal_stats <- get_map_stats(x = x_input,
                                temporal = temporal_input,
                                keep_max = FALSE,
                                keep_visuals = TRUE)

# converting land from square meters to square miles
temporal_stats_plot <- temporal_stats |>
  dplyr::mutate(land = land/2.59e6)

territoryracing(x = temporal_stats_plot,
                stat_name = "land",
                title = "2022 Season Week by Week - Undefeated CFB Territory Map",
                subtitle = "Area in Square Miles",
                caption = "Data Source: cfbd.com",
                output_file = "C:/Users/lwget/Downloads/cfb_undefeated_s2022_racing.gif")
