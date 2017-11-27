#' A degree conversion function
#'
#' This function converts degrees to radians.
#' @param deg Degree unit to be converted to radian.
#' @keywords degree units
#' @export
#' @examples
#' deg_rad(360)


deg_rad <- function(deg) return(deg*pi/180)
