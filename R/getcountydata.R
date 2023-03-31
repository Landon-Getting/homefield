#' getcountydata
#'
#' @importFrom magrittr %>%
#' @importFrom tigris counties
#' @import leaflet
#' @return nothing
#'
#' @export
#'
getcountydata <- function() {

  counties = counties()

  leaflet::leaflet(options = leaflet::leafletOptions(crs = epsg2163,
                                      zoomControl = TRUE,
                                      zoomSnap = 0.25,
                                      zoomDelta = 1),
  height = 2000,
  width = 3200) %>%
  leaflet::setView(lng = -98.64580,
                   lat = 38.05909,
                   zoom = 5) %>%
  leaflet::addPolygons(data = counties,
                       color = "black",)
}
