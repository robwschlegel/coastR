#' A degree conversion function
#'
#' This function converts degrees to radians.
#' @param deg Degree unit to be converted to radian.
#' @keywords degree units
#' @export
#' @examples
#' deg2rad(360)
deg2rad <- function(deg) return(deg*pi/180)