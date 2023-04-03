library(hexSticker)
s <-sticker("C:/Users/lwget/Downloads/flag-solid.png",
            package="territorymaps",
            p_size=20,
            s_x=1,
            s_y=.75,
            s_width=.4,
            s_height = .4,
            filename="inst/figures/imgfile.png")

ggplot2::ggsave("C:/Users/lwget/Downloads/territorymap_tempsticker.png", plot = s, width = 8, height = 8, dpi = 300)
