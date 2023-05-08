``` r
library(sf)
#> Warning: package 'sf' was built under R version 4.1.3
#> Linking to GEOS 3.10.2, GDAL 3.4.1, PROJ 7.2.1; sf_use_s2() is TRUE
library(leaflet)
#> Warning: package 'leaflet' was built under R version 4.2.2

# loading spatial data
spatial_data <- st_read(system.file("shape/nc.shp", package="sf")) |>
  sf::st_transform("+proj=longlat +datum=WGS84")
#> Reading layer `nc' from data source 
#>   `C:\Users\lwget\Documents\R\win-library\4.1\sf\shape\nc.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

# gathering a single polygon as an example from spatial data
example_polygon <- spatial_data[1,15]

#  polygon centroid point
polygon_centroid <- sf::st_centroid(example_polygon)

# distance from polygon centroid to closest point on polygon (as linestring)
desired_distance_linestring <- sf::st_nearest_points(example_polygon,
                                             polygon_centroid,
                                             pairwise = TRUE)

# returns desired distance in meters rather than as a linestring
# NEED PIXELS RATHER THAN METERS
#desired_distance <- sf::st_length(desired_distance_linestring)

# Creating leaflet map
leaflet::leaflet() |>
  # plotting example polygon
  leaflet::addPolygons(data = example_polygon,
                       fillOpacity = 0,
                       color = "red",
                       weight = 1) |>
  # visualizing desired distance
  leaflet::addPolygons(data = desired_distance_linestring,
                       fillOpacity = 0,
                       color = "blue",
                       weight = 1)
```

![](https://i.imgur.com/WtuPSBR.png)<!-- -->

<sup>Created on 2023-05-03 with [reprex v2.0.2](https://reprex.tidyverse.org)</sup>
