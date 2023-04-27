#' homefield_shiny
#'
#' @description Opens the homefield Shiny App and features college football homefield maps.
#'
#' @importFrom shiny runApp
#'
#' @export
#'
#' @examples
#' \dontrun{homefield::homefield_shiny()}
homefield_shiny <- function(){

  shiny::runApp(appDir = system.file("app", package = "homefield"))

}
