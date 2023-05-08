entity <- c("Iowa State Cyclones", "Florida Gators", "UCLA Bruins")

# must be valid locations
lat <- c(42.01400, 29.64994, 34.16133)
lng <- c(-93.63577, -82.34858, -118.16765)

# must be a color in hexcode format
color <- c("#660015","#0021A5", "#ffc72c")

# can be a link or local file
image <- c("http://a.espncdn.com/i/teamlogos/ncaa/500/66.png",
           "http://a.espncdn.com/i/teamlogos/ncaa/500/57.png",
           "http://a.espncdn.com/i/teamlogos/ncaa/500/26.png")

homefield_data <- data.frame(entity,
                             lat,
                             lng,
                             color,
                             image)

library(ggplot2)

helpful_stats <- homefield_stats(homefield_data,
                                 keep_visuals = TRUE)

helpful_stats <- helpful_stats |>
  dplyr::mutate(land = land/2.59e6,
                water = water/2.59e6,
                domain = domain/2.59e6)

myColors <- c("#046a38", "#2774AE", "#C8102E")

helpful_stats$entity <- factor(helpful_stats$entity,  # Factor levels in decreasing order
                  levels = helpful_stats$entity[order(helpful_stats$land, decreasing = FALSE)])

ggplot(helpful_stats, aes(x=land, y=entity, fill = entity)) +
  geom_col(width = 0.5) +
  xlab("Land (in Miles Squared)") +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = myColors) +
  ylab("") +
  theme(legend.position = "none",
        axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold")) +
  theme(plot.margin = margin(0.4,1.2,0.4,0.4, "cm"))

