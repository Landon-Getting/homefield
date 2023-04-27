# ui object
fluidPage(
  titlePanel(p("homefield - Interactive CFB Map", style = "color:#3474A7")),
  sidebarLayout(
    sidebarPanel(
      column(width = 12, align = "center",img(src='sticker.png',height = "200px")),
      helpText(
        "Welcome to the homefield Shiny App!
        There are two college football datasets to visualize.
        In `Undefeated`, unbeaten teams control the counties closest to them.
        In `Conquest`, teams gain another team's territory when they defeat them."
      ),
      selectInput(
        inputId = "mapType",
        label = "Select a type of map",
        choices = c("Undefeated",
                    'Conquest'),
        selected = "Undefeated"
      ),
      selectInput(
        inputId = "season",
        label = "Select a season",
        choices = sort(1950:2022, decreasing = TRUE),
        selected = 2022
      ),
      selectInput(
        inputId = "week",
        label = "Select a week",
        choices = 0:14,
        selected = 0
      ),
      actionButton("generate_map", "Generate Map")
    ),
    mainPanel(
      leaflet::leafletOutput(outputId = "map",
                             height = 600)
    )
  )
)
