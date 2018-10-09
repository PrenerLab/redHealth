#' Calculate Redlined Area per Tract
#'
#' @description This function takes a census tract input as well as a HOLC redlining input and
#'    calculates the areas per census tract that have been redlined for a given category.
#'
#' @param tract \code{sf} object with census tract geometry
#' @param holc \code{sf} object with redlining geometry
#' @param cat string input that is one of \code{"A"}, \code{"B"}, \code{"C"}, or \code{"D"}
#'
#' @return A tibble with a row for each tract GEOID and the corresponding area redlined for a given category.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom sf st_geometry
#' @importFrom sf st_intersection
#'
#' @export
rh_area <- function(tract, holc, cat){

  # subset so that we only calculate the area for one HOLC category
  y <- dplyr::filter(holc, holc_grade == cat)

  # perform intersection
  xy <- sf::st_intersection(x = tract, y = y)

  # calculate area of features
  xy <- dplyr::mutate(xy, AREA = sf::st_area(xy))

  # remove geometric information
  sf::st_geometry(xy) <- NULL

  # group and summarize
  xy %>%
    dplyr::mutate(AREA = as.numeric(AREA)) %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarise(AREA = sum(AREA)) -> out

  # ensure output is formatted as tibble
  out <- dplyr::as_tibble(out)

  # return output
  return(out)

}
