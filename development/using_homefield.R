library(homefield)

cfb_undefeated_s1999_w0 <- homefield::cfb_undefeated(season = 1999, week = 0)

homefield_map(x = cfb_undefeated_s1999_w0,
             output_file = "C:/Users/lwget/Downloads/cfb_undefeated_s2022_w0.png",
             title = "College Football Undefeated - Season 1999 Week 0",
             credit = "Landon Getting")


for(i in 0:15){
  homefield_map(x = cfb_undefeated(season = 2022, week = i),
               output_file = paste0("C:/Users/lwget/Downloads/cfb_undefeated_s2022_w", i,".png"),
               title = paste0("College Football Undefeated - Season 2022 Week ", i),
               credit = "Landon Getting")
}

cfb_undefeated_s2022_w6_stats <- homefield_stats(x = cfb_undefeated_s2022_w6)

View(cfb_undefeated_s2022_w6_stats)

# what was the most land acquired by a team?
max(cfb_undefeated_s2022_w6_stats$land)


# let's look at land over time for the 2022 season

# undefeated for each week, 0 through 15
x_input <- list(cfb_undefeated(season = 2022, week = 0),
                cfb_undefeated(season = 2022, week = 1),
                cfb_undefeated(season = 2022, week = 2),
                cfb_undefeated(season = 2022, week = 3),
                cfb_undefeated(season = 2022, week = 4),
                cfb_undefeated(season = 2022, week = 5),
                cfb_undefeated(season = 2022, week = 6),
                cfb_undefeated(season = 2022, week = 7),
                cfb_undefeated(season = 2022, week = 8),
                cfb_undefeated(season = 2022, week = 9),
                cfb_undefeated(season = 2022, week = 10),
                cfb_undefeated(season = 2022, week = 11),
                cfb_undefeated(season = 2022, week = 12),
                cfb_undefeated(season = 2022, week = 13),
                cfb_undefeated(season = 2022, week = 14),
                cfb_undefeated(season = 2022, week = 15))

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

temporal_stats <- homefield_stats(x = x_input,
                                temporal = temporal_input,
                                keep_max = FALSE,
                                keep_visuals = TRUE)

# converting land from square meters to square miles
temporal_stats_plot <- temporal_stats |>
  dplyr::mutate(land = land/2.59e6)

x <- temporal_stats_plot
stat_name = "land"
title = "2022 Season Week by Week - Undefeated CFB homefield Map"
subtitle = "Area in Square Miles"
caption = "Data Source: cfbd.com"
output_file = "C:/Users/lwget/Downloads/cfb_undefeated_s2022_racing.gif"

homefield_racing(x = temporal_stats_plot,
                stat_name = "land",
                title = "2022 Season Week by Week - Undefeated CFB homefield Map",
                subtitle = "Area in Square Miles",
                caption = "Data Source: cfbd.com",
                output_file = "C:/Users/lwget/Downloads/cfb_undefeated_s2022_racing.gif")