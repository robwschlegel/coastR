#' Coastal Transects
#'
#' This function finds the correct angle for transects that
#' run either along or away from a set of points along a coastline.
#'
#' @param site_list A dataframe with a 'lon' and 'lat' column that
#' contains coordinates for sites along a coastline.
#' @param alongshore A boolean TRUE/FALSE to determine if the
#' transects should be shore-normal or alongshore. Default = FALSE.
#' @param spread The number of pixels along the coast on either
#' side of the site in question that should be used to determine
#' the angle of the shore-normal transect. A larger spread value
#' will provide smoother angles, which may be undesirable for
#' convoluted coastlines. Defaut = 2.
#' @param coast Should only the coastline be used to calculate the
#' angle for transects? Or should islands be included? Calculating
#' the angle of transects form or along the faces of islands is
#' advised against. The coastline behind the island should almost
#' always be used instead. Default = TRUE.
#' @param reverse A boolean TRUE/FALSE that will reverse the angle
#' of the shore-normal transects. Default = FALSE.
#' @return A dataframe with the correct ordering of the input sites
#' @keywords transects alongshore shore-normal
#' @export
#' @examples
#' SACTN_site_list_transects <- transects(SACTN_site_list[25:45,])

transects <- function(site_list, alongshore = FALSE, spread = 2, coast = TRUE, reverse = FALSE) {

  # Check for lon/lat columns
  if(is.null(site_list$lon)) return("Please ensure a 'lon' column is provided.")
  if(is.null(site_list$lat)) return("Please ensure a 'lat' column is provided.")

  # Give a warning message if a heading column exists in 'site_list' as it will be overwritten
  if(!(is.null(site_list$heading))) {
    site_list$heading <- NULL
    warning("The 'angle' column has been overwritten.")
  }

  # Fetch coastline values
  coastline <- ggplot2::fortify(maps::map(xlim = c(min(site_list$lon-1, na.rm = T),
                                                   max(site_list$lon+1, na.rm = T)),
                                          ylim = c(min(site_list$lat-1, na.rm = T),
                                                   max(site_list$lat+1, na.rm = T)),
                                          plot = F,  interior = F, fill = T,
                                          lforce = "e", map = "world"))

  # Remove islands as desired
  if(coast){
    coastline <- coastline[base::is.na(coastline$subregion),]
  }

  # Extract coastal coordinates
  lon2 <- akima::aspline(coastline$long, n = length(coastline$long)*10)$y
  lat2 <- akima::aspline(coastline$lat, n = length(coastline$lat)*10)$y

  # Find the site on the coastline and it's nearest neighbour points
  lon <- NULL; lat <- NULL
  coords1 <- data.frame(lon = lon2, lat = lat2)
  coords2 <- FNN::knnx.index(as.matrix(coords1),
                             as.matrix(dplyr::select(site_list, lon, lat)), k = 1)
  coords3 <- data.frame(lon1 = lon2[coords2-spread],
                        lat1 = lat2[coords2-spread],
                        lon2 = lon2[coords2+spread],
                        lat2 = lat2[coords2+spread])

  # Define the alongshore transect bearing
  heading1 <- fossil::earth.bear(coords3[1,1], coords3[1,2], coords3[2,1], coords3[2,2])
  heading2 <- fossil::earth.bear(coords3$lon1[1], coords3$lat1[1], coords3$lon2[1], coords3$lat2[1])
  heading3 <- fossil::earth.bear(coords3$lon1, coords3$lat1, coords3$lon2, coords3$lat2)
  heading4 <- fossil::earth.bear(lon2[coords2-spread], lat2[coords2-spread],
                                 lon2[coords2+spread], lat2[coords2+spread])

  # Convert to shore-normal as desired
  if(!(alongshore)){
    heading4 <- heading4-90
    heading4[heading4 >= 360] <- heading4[heading4 >= 360]-360
  }

  # Add angle column and finish
  res <- data.frame(site_list, heading = heading4)
  return(res)
}
