x_input <- list(get_cfb_undefeated(season = 2021, week = 0),
                 get_cfb_undefeated(season = 2021, week = 1),
                 get_cfb_undefeated(season = 2021, week = 2),
                 get_cfb_undefeated(season = 2021, week = 3),
                 get_cfb_undefeated(season = 2021, week = 4),
                 get_cfb_undefeated(season = 2021, week = 5),
                 get_cfb_undefeated(season = 2021, week = 6),
                 get_cfb_undefeated(season = 2021, week = 7),
                 get_cfb_undefeated(season = 2021, week = 8),
                 get_cfb_undefeated(season = 2021, week = 9),
                 get_cfb_undefeated(season = 2021, week = 10),
                 get_cfb_undefeated(season = 2021, week = 11),
                 get_cfb_undefeated(season = 2021, week = 12),
                 get_cfb_undefeated(season = 2021, week = 13),
                 get_cfb_undefeated(season = 2021, week = 14))

# week 0 through 14
temporal_input <- c(0,
                    1,
                    2,
                    3,
                    4,
                    5,
                    6,
                    7,
                    8,
                    9,
                    10,
                    11,
                    12,
                    13,
                    14)


temporal_stats <- get_map_stats(x = x_input,
                                temporal = temporal_input,
                                keep_max = FALSE)



clean_teams <- function(teams){
  alt_color_list = c("Akron",
                     "Baylor",
                     "Charlotte",
                     "Houston",
                     "Iowa",
                     "Kent State",
                     "LSU",
                     "Maryland",
                     "Minnesota",
                     "Montana",
                     "NC State",
                     "North Texas",
                     "Northwestern",
                     "SMU",
                     "Temple",
                     "Tennessee",
                     "Tulsa",
                     "UC Davis",
                     "UCLA",
                     "UMass",
                     "USC",
                     "Utah State",
                     "Wisconsin",
                     "Ohio State",
                     "Purdue")

  alt_logo_list = c("Air Force",
                    "Alabama",
                    "Boston College",
                    "BYU",
                    "California",
                    "Cincinnati",
                    "Clemson",
                    "Duke",
                    "Indiana",
                    "James Madison",
                    "Kansas State",
                    "Kentucky",
                    "LSU",
                    "Michigan",
                    "Michigan State",
                    "Nevada",
                    'Oklahoma',
                    "Oregon",
                    "Pittsburgh",
                    "Rice",
                    "San Diego State",
                    "TCU",
                    "Texas",
                    "Texas A&M",
                    "Wake Forest",
                    "Washington State")

  # getting best colors
  teams <- teams |>
    dplyr::mutate(best_color = dplyr::case_when(
      .data$school %in% alt_color_list ~ alt_color,
      !.data$school %in% alt_color_list ~ color
    ))

  # getting best logos
  teams <- teams |>
    dplyr::mutate(best_logo = dplyr::case_when(
      .data$school %in% alt_logo_list ~ logo_2,
      !.data$school %in% alt_logo_list ~ logo
    ))

  return(teams)
}


# NEW SOLUTION FROM SCRATCH W GGANIMATE
library(tidyverse)
library(gganimate)
library(gifski)
library(readr)
library(ggpattern)

temporal_stats_plot <- temporal_stats |>
  dplyr::rename(week = time,
                school = identifier)

teams <- cfbfastR::cfbd_team_info(only_fbs = TRUE, year = 2021) |>
  clean_teams() |>
  dplyr::select(school,
                best_color,
                best_logo)

if (any(is.na(teams$best_color))) {
  # Fill in missing values with white
  teams$best_color[is.na(teams$best_color)] <- "#FFFFFF"
}

# checking for missing logos
if (any(is.na(teams$best_logo))) {
  # Fill in missing values with NCAA football logo
  teams$best_logo[is.na(teams$best_logo)] <- "./inst/figures/no_logo.png"
}

temporal_stats_plot <- dplyr::left_join(temporal_stats_plot,
                                        teams,
                                        by = "school")

# inspired by https://rpubs.com/haginta/709479

temporal_stats_set <- temporal_stats_plot |>
  dplyr::group_by(week) |>
  dplyr::mutate(rank = rank(-land),
         school_rel = land/land[rank==1],
         school_lbl = paste0(" ",round(land/1e6))) |>
  dplyr::group_by(school) |>
  dplyr::filter(rank <=10) |>
  dplyr::ungroup() |>
  dplyr::arrange(week, rank)

static_plot <- ggplot(temporal_stats_set,
       aes(group = school)) +
  geom_rect_pattern(
    aes(pattern_filename = I(best_logo),
        fill = best_color,
        ymin = 0,
        ymax = land,
        xmin = rank - 0.45,
        xmax = rank + 0.45),
    pattern         = "image",
    pattern_type    = 'none',
    pattern_scale   = -2,
    pattern_gravity = 'east') +
  theme(legend.position = 'none') +
  scale_fill_identity() +
  geom_text(aes(x = rank,
            y = 0,
                label = paste(school, " ")),
            vjust = 0.2,
            hjust = 1) +
  geom_text(aes(x = rank,
                y=land,
                label = school_lbl,
                hjust=0)) +
  geom_text(aes(x=10,
                y=max(land),
                label = as.factor(week)),
            vjust = 0.2,
            alpha = 0.5,
            col = "gray",
            size = 20) +
  coord_flip(clip = "off",
             expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line(linewidth=.1,
                                          color="grey" ),
        panel.grid.minor.x = element_line(linewidth=.1,
                                          color="grey" ),
        plot.title = element_text(size=25,
                                  hjust=0,
                                  face="bold",
                                  colour="black",
                                  vjust=-1),
        plot.subtitle = element_text(size=18,
                                     hjust=1,
                                     face="italic",
                                     color="grey"),
        plot.caption = element_text(size=14,
                                    hjust=1,
                                    face="italic",
                                    color="grey"),
        plot.background = element_blank(),
        plot.margin = margin(2, 2, 2, 4, "cm"))


animated <- static_plot + transition_states(week,
                                            transition_length = 8,
                                            state_length = 2,
                                            wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  ease_aes('linear')+
  enter_fade()+
  exit_fade() +
  labs(title = 'CFB Teams Territory by Territory - Season 2021 Undefeated Map',
       subtitle  =  "Area in Square Kilometers",
       caption  = "Data Source: cfbd.com")

animate(animated, 600, fps = 20, end_pause = 30, width = 1500, height = 1000,
        renderer = gifski_renderer("C:/Users/lwget/Downloads/racing.gif"))








































