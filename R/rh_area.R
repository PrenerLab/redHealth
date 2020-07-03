#' Calculate Redlined Area per Tract
#'
#' @description This function takes a census tract input as well as a HOLC redlining input and
#'    calculates the areas per census tract that have been redlined for a given category.
#'
#' @param areal_unit \code{sf} object with census GEOID column
#' @param holc \code{sf} object with redlining geometry
#' @param cat string input that is one of \code{"A"}, \code{"B"}, \code{"C"}, or \code{"D"} or
#'     a vector like \code{c("A", "B")}
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
rh_area <- function(areal_unit, holc, cat){

  # global bindings
  holc_grade = HOLC_AREA = GEOID = NULL

  # subset so that we only calculate the area for one HOLC category
  if (length(cat) == 1){
    focal_area <- dplyr::filter(holc, holc_grade == cat)
  } else if (length(cat) > 1){
    focal_area <- dplyr::filter(holc, holc_grade %in% cat)
  }

  # perform intersection
  out <- sf::st_intersection(x = areal_unit, y = focal_area)

  # calculate area of features
  out <- dplyr::mutate(out, HOLC_AREA = sf::st_area(out))

  # remove geometric information
  sf::st_geometry(out) <- NULL

  # group and summarize
  out <- dplyr::mutate(out, HOLC_AREA = as.numeric(HOLC_AREA))
  out <- dplyr::group_by(out, GEOID)
  out <- dplyr::summarise(out, HOLC_AREA = sum(HOLC_AREA))

  # ensure output is formatted as tibble
  out <- dplyr::as_tibble(out)

  # return output
  return(out)

}
