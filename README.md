
# homefield

## Overview

Creates homefield maps to visualize representation of entities by
region. ![](./inst/figures/example_map.png)

## Installation

Install the package via GitHub:

``` r
install_github("Landon-Getting/homefield")
```

Follow the directions
[https://cfbfastr.sportsdataverse.org/#college-football-data-api-keys](here)
to receive and install a College Football Data API Key. Required for
`homefield::cfb_undefeated()` and `homefield::cfb_conquest()`.

Follow the directions
[https://walker-data.com/tidycensus/reference/census_api_key.html](here)
to receive and install a CENSUS API Key. Required for
`homefield::homefield_stats()`.

# WORK IN PROGRESS BELOW

## Input Data Frame:

| Identifier      | Latitude  | Longitude  | Color    | Image                                                          |
|-----------------|-----------|------------|----------|----------------------------------------------------------------|
| Dodgers         | 42.501171 | -94.169388 | \#bf250d | C:/Users/darthvader/Documents/PlotPictures/Dodgers.jpg         |
| Little Cyclones | 42.501171 | -94.169388 | \#eda01a | C:/Users/darthvader/Documents/PlotPictures/Little-Cyclones.jpg |
| Rams            | 42.501171 | -93.464610 | \#ffe924 | C:/Users/darthvader/Documents/PlotPictures/Rams.jpg            |

## Output:

Territory map where each element is represented on a map by the closest
regions (counties/states/countries) to the input lat and long. This
calculation is performed based on the centroid of each region. Regions
are colored based on provided colors in column of the data frame. The
image from each element is placed into its respective territory. The
image does not touch the boundaries of the territory and fits inside
nicely.

## Use Cases:

**Athletic -** What teams or individuals are currently undefeated and
who is their closest geographic competition?

**Economic -** Where is the closest wholesale store to each US county
(Sam’s Club vs CostCo)?

**Commerical -** Show the LinkedIn headshot for the closest sales rep to
each state.

**Political -** What is the closest private high school to each
Minnesota county? What is the closest public high school to each
Wisconsin county?

*Inspired by
[u/CaptainScuttlebottom](https://www.reddit.com/user/CaptainScuttlebottom/)
and [u/jloose128](https://www.reddit.com/user/jloose128/) on Reddit*
