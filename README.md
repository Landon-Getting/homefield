
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/Landon-Getting/homefield/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Landon-Getting/homefield/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Overview <img src="man/figures/logo.png" align="right" height="138" />

The homefield package helps gather data and create visuals to show
spatial relevance of entities.

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/undefeated_map.png?raw=true)

------------------------------------------------------------------------

## Installation

1.  Install [R and R
    Studio](https://posit.co/download/rstudio-desktop/).

2.  Install the latest compatible version of
    [RTools](https://cran.r-project.org/bin/windows/Rtools/).

3.  Install the devtools package.

``` r
install.packages("devtools")
```

4.  Install the homefield package via Github.

``` r
devtools::install_github("Landon-Getting/homefield")
```

*homefield relies on two online databases for up-to-date college
football and census information. These databases require API keys which
can be requested and installed via the following instructions:*

5.  Follow the directions
    [here](https://cfbfastr.sportsdataverse.org/#college-football-data-api-keys)
    to receive and install a College Football Data API Key. Required for
    `homefield::cfb_undefeated()` and `homefield::cfb_conquest()`.

6.  Follow the directions
    [here](https://walker-data.com/tidycensus/reference/census_api_key.html)
    to receive and install a CENSUS API Key. Required for
    `homefield::homefield_stats()`.

7.  Once the package and keys have been installed, homefield can be
    initialized in an environment via `library()`:

``` r
library(homefield)
# your code here
```

------------------------------------------------------------------------

## homefield_map()

homefield maps are created based on the attributes of **entities**
including identifier, latitude, longitude, color, and image.

For instance, a map may be composed of sport teams. The Iowa State
Cyclones **entity** would have an *entity identifier* (`Iowa State`), a
*latitude* (`42.0266573`), a *longitude* (`-93.6464516`), a *color*
(`#C8102E`), and an *image*
(`http://a.espncdn.com/i/teamlogos/ncaa/500/66.png`).

A dataframe with multiple entities and their attributes may look similar
to the following:

``` r
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

homefield_data
```

    ##                entity      lat        lng   color
    ## 1 Iowa State Cyclones 42.01400  -93.63577 #660015
    ## 2      Florida Gators 29.64994  -82.34858 #0021A5
    ## 3         UCLA Bruins 34.16133 -118.16765 #ffc72c
    ##                                              image
    ## 1 http://a.espncdn.com/i/teamlogos/ncaa/500/66.png
    ## 2 http://a.espncdn.com/i/teamlogos/ncaa/500/57.png
    ## 3 http://a.espncdn.com/i/teamlogos/ncaa/500/26.png

Next, the homefield map can be plotted with the dataframe and saved as a
.png file to a specified location.

``` r
homefield_map(x = homefield_data, # dataframe
             output_file = "C:/Users/darthvader/Downloads/example_map.png", # save location
             threshold = 10000, # area in square km
             title = "Example Map", # map title
             credit = "Landon Getting") # credit for author
```

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/example_map.png?raw=true)

------------------------------------------------------------------------

## Getting data for homefield maps

The homefield package currently provides 2 functions to query
interesting college football data directly into the structured format:
`cfb_undefeated()` and `cfb_conquest()`. Remember to complete the steps
in *Installation* to receive and install an College Football Database
API key.

### cfb_undefeated()

`cfb_undefeated()` returns a dataframe with the undefeated teams for a
particular season and week.

``` r
cfb_undefeated_s1999_w4 <- cfb_undefeated(season = 1999, week = 4)

head(cfb_undefeated_s1999_w4)
```

    ##           entity      lat        lng   color
    ## 1      Air Force 38.99697 -104.84362 #004a7b
    ## 2       Arkansas 36.06807  -94.17895 #9c1831
    ## 3         Auburn 32.60255  -85.48975 #03244d
    ## 4            BYU 40.25753 -111.65452 #001E4C
    ## 5 Boston College 42.33510  -71.16644 #88001a
    ## 6  East Carolina 35.59685  -77.36456 #4b1869
    ##                                                     image
    ## 1 http://a.espncdn.com/i/teamlogos/ncaa/500-dark/2005.png
    ## 2         http://a.espncdn.com/i/teamlogos/ncaa/500/8.png
    ## 3         http://a.espncdn.com/i/teamlogos/ncaa/500/2.png
    ## 4  http://a.espncdn.com/i/teamlogos/ncaa/500-dark/252.png
    ## 5  http://a.espncdn.com/i/teamlogos/ncaa/500-dark/103.png
    ## 6       http://a.espncdn.com/i/teamlogos/ncaa/500/151.png

The dataframe output from `cfb_undefeated()` can be used directly in
`homefield_map()`.

``` r
homefield_map(x = cfb_undefeated(season = 1999, week = 4),
             output_file = "C:/Users/darthvader/Downloads/cfb_undefeated_s2022_w4.png",
             title = "College Football Undefeated - Season 1999 Week 4",
             credit = "Landon Getting")
```

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/undefeated_map.png?raw=true)

### cfb_conquest()

In `cfb_conquest()`, teams start with the land closest to them. As the
season progresses, teams acquire the land of the teams they defeat.

``` r
cfb_conquest_s1999_w4 <- cfb_conquest(season = 1999, week = 4)

head(cfb_conquest_s1999_w4)
```

    ##      entity      lat        lng   color
    ## 1 Air Force 38.99697 -104.84362 #004a7b
    ## 2     Akron 42.99913  -78.77751 #84754e
    ## 3  Arkansas 36.06807  -94.17895 #9c1831
    ## 4  Arkansas 32.83772  -96.78279 #9c1831
    ## 5    Auburn 36.21143  -81.68543 #03244d
    ## 6    Auburn 32.60255  -85.48975 #03244d
    ##                                                     image
    ## 1 http://a.espncdn.com/i/teamlogos/ncaa/500-dark/2005.png
    ## 2      http://a.espncdn.com/i/teamlogos/ncaa/500/2006.png
    ## 3         http://a.espncdn.com/i/teamlogos/ncaa/500/8.png
    ## 4         http://a.espncdn.com/i/teamlogos/ncaa/500/8.png
    ## 5         http://a.espncdn.com/i/teamlogos/ncaa/500/2.png
    ## 6         http://a.espncdn.com/i/teamlogos/ncaa/500/2.png

The dataframe output from `cfb_conquest()` can be used directly in
`homefield_map()`.

``` r
homefield_map(x = cfb_conquest(season = 1999, week = 4),
             output_file = "C:/Users/darthvader/Downloads/cfb_conquest_s2022_w4.png",
             title = "College Football Conquest - Season 1999 Week 4",
             credit = "Landon Getting")
```

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/conquest_map.png?raw=true)

------------------------------------------------------------------------

## Summarizing Maps

`homefield_stats()` generates summary statistics about a particular map
including the total land, water, and population within each entity’s
territory.

The same dataframe input used in `homefield_map()` can be used in
`homefield_stats()`.

``` r
cfb_undefeated_s1999_w4_stats <- homefield_stats(x = cfb_undefeated(season = 1999, week = 4))

head(cfb_undefeated_s1999_w4_stats)
```

    ##           entity         land       water       domain      pop
    ## 1      Air Force 610234978656  2533273302 612768251958  6638127
    ## 2       Arkansas 186833484833  4234865393 191068350226  5122951
    ## 3         Auburn 106215319059  4688840187 110904159246  4365585
    ## 4            BYU 292222970451  1844563724 294067534175  1448537
    ## 5 Boston College 223771886456 31278621299 255050507755 31475972
    ## 6  East Carolina 104201019210 15782222629 119983241839  7592900

### Map Stats over Time

`homefield_stats()` can also create summary statistics for maps over
time using the *temporal* argument. For example, `homefield_stats()` can
be combined with `cfb_undefeated()` to show statistics over the course
of a season.

``` r
# undefeated for each week, 0 through 15
cfb_undefeated_2022 <- list(cfb_undefeated(season = 2022, week = 0),
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

# date of each week
cfb_dates_2022 <- lubridate::ymd(c("2022-08-27",
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

cfb_undefeated_2022_stats <- homefield_stats(x = cfb_undefeated_2022,
                                  temporal = cfb_dates_2022,
                                  keep_max = FALSE,
                                  keep_visuals = TRUE)
```

Converting the output of square meters to square miles.

``` r
# converting land from square meters to square miles
cfb_undefeated_2022_stats <- cfb_undefeated_2022_stats |>
  dplyr::mutate(land = land/2.59e6) |>
  dplyr::select(entity,
                land,
                time,
                color,
                image)

head(cfb_undefeated_2022_stats)
```

    ##              entity      land       time   color
    ## 1         Air Force 73410.774 2022-08-27 #004a7b
    ## 2             Akron  5384.247 2022-08-27 #84754e
    ## 3           Alabama  9698.773 2022-08-27 #690014
    ## 4 Appalachian State 11482.544 2022-08-27 #000000
    ## 5           Arizona 23098.589 2022-08-27 #002449
    ## 6     Arizona State 66025.140 2022-08-27 #942139
    ##                                                     image
    ## 1 http://a.espncdn.com/i/teamlogos/ncaa/500-dark/2005.png
    ## 2      http://a.espncdn.com/i/teamlogos/ncaa/500/2006.png
    ## 3  http://a.espncdn.com/i/teamlogos/ncaa/500-dark/333.png
    ## 4      http://a.espncdn.com/i/teamlogos/ncaa/500/2026.png
    ## 5        http://a.espncdn.com/i/teamlogos/ncaa/500/12.png
    ## 6         http://a.espncdn.com/i/teamlogos/ncaa/500/9.png

------------------------------------------------------------------------

## Visualizing Stats

`homefield_racing()` generates a racing bar chart animation where
entities compete for the top 10 spots based on summary statistics from
`homefield_stats()`.

For example, the output dataframe from `homefield_stats()` describing
the 2022 college football season can be visualized with the
`homefield_racing()` function.

``` r
homefield_racing(x = cfb_undefeated_2022_stats,
                stat_name = "land",
                title = "2022 Season Week by Week - Undefeated CFB homefield Map",
                subtitle = "Area in Square Miles",
                caption = "Data Source: cfbd.com",
                output_file = "C:/Users/lwget/Downloads/cfb_undefeated_2022_racing.gif")
```

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/homefield_racing.gif?raw=true)

------------------------------------------------------------------------

## Interactive Maps

`homefield_shiny()` showcases several functions from the homefield
package in an interactive application visualizing College Football
seasons. Users can choose a type of map, season, and week and the
homefield map will be automatically generated.

![](https://github.com/Landon-Getting/homefield-graphics/blob/main/homefield_shiny.png?raw=true)

## Acknowledgements

Thank you to [Matt
Daniel](https://www.linkedin.com/in/matt-daniel-8b03b1196/) - designer
of the homefield sticker.

The homefield package was created as a part of [Dr. Heike
Hofmann’s](https://heike.github.io/) STAT 585 at Iowa State University.

homefield was inspired by similar maps created by
[u/CaptainScuttlebottom](https://www.reddit.com/user/CaptainScuttlebottom/)
and [u/jloose128](https://www.reddit.com/user/jloose128/) on Reddit.
