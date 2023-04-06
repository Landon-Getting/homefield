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

temporal_stats <- territorymap::get_map_stats(x = x_input, temporal = temporal_input)

temporal_stats <- temporal_stats_backup

temporal <- temporal_input


# check to see if an identifier is not present in future instances
# get list of identifiers in temporal[[1]]



temporal_stats_top <- temporal_stats |>
  dplyr::group_by(identifier) |>
  dplyr::mutate(max_pop = max(pop)) |>
  dplyr::ungroup() |>
  dplyr::filter(time == max(time)) |>
  dplyr::top_n(n = 10, wt = max_pop) |>
  dplyr::select(identifier)

temporal_stats_plot <- temporal_stats |>
  dplyr::filter(identifier %in% temporal_stats_top$identifier)

# CURRENT SOLUTION ------------------------------------------------------------
ddplot::barChartRace(temporal_stats_plot,
                     x = "pop",
                     y = "identifier",
                     time = "time",
                     transitionDur = 2000)


# ONLINE SOLUTION --------------------------------------------------------------
# filter out only top 10 populations for each week
temporal_formatted <- temporal_stats %>%
  group_by(time) %>% # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-pop),
         Value_rel = pop/pop[rank==1],
         Value_lbl = paste0(" ",round(pop/1e6))) %>%
  group_by(identifier) %>%
  filter(rank <=10) %>%
  ungroup()

staticplot = ggplot(temporal_formatted, aes(rank, group = identifier,
                                       fill = as.factor(identifier), color = as.factor(identifier))) +
  geom_tile(aes(y = pop/2,
                height = pop,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(identifier, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=pop,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( linewidth=.1, color="grey" ),
        panel.grid.minor.x = element_line( linewidth=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(time,
                                      transition_length = 4,
                                      state_length = 2) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'CFB Undefeated Territory Map - Season 2022 Week: {closest_state}',
       subtitle  =  "Top 10 Schools",
       caption  = "Population in Milllions")

animate(anim, 200, fps = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("C:/Users/lwget/Downloads/gganim.gif"))







































