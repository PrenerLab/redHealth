#' Combined Measured Areas with Tract Boundaries
#'
#' @description This function takes a census tract output as well as the returned results of
#'     \code{rh_area} and combines them to provide area measurements for each feature regardless
#'     of if it was redlined or not.
#'
#' @param areal_unit \code{sf} object with census GEOID column
#' @param area A tbl or data frame containing area of redlining per tract for a given grade
#' @param by Variable name common to both \code{tract} and \code{area} (such as \code{GEOID})
#' @param cat Category name to be given to new variable in the returned \code{sf} object
#'
#' @return An \code{sf} object with a column containing the area covered by a given grade.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom rlang :=
#'
#' @export
rh_area_join <- function(areal_unit, area, by, cat){

  # set global bindings
  HOLC_AREA = NULL

  # join data
  out <- dplyr::left_join(areal_unit, area, by = by)

  # rename variable and fill in zeros
  out <- mutate(out, HOLC_AREA = ifelse(is.na(HOLC_AREA) == TRUE, 0, HOLC_AREA))
  out <- rename(out, !!cat := HOLC_AREA)

  # return output
  return(out)

}
