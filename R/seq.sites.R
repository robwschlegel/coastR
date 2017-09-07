#' Sequential coastal site order
#'
#' This function finds the order of sites along a convoluted coast
#' @param site_list A dataframe with a 'lon' and 'lat' column that
#' contains coordinates along a coastline that need to be ordered
#' @keywords sequential sites
#' @export
#' @examples
#' seq.sites()

# load("~/SACTNraw/metadata/site_list_v4.1.Rdata")
# site_list <- site_list[17:32,]
# tester <- data.frame()
# lon <- site_list$lon
# lat <- site_list$lat

# x <- seq(1, 20, 2)
# y <- rnorm(10)
# f <- approx(lon)
# f <- xy.coords(lon)
# f <- akima::aspline(lon, n = length(lon)*2)


seq.sites <- function(site_list, coast = NULL){

  # Create site list

  # Check for lon/lat columns

  # Fetch coastline values
  coastline <- maps::map(xlim = c(min(site_list$lon, na.rm = T),
                                         max(site_list$lon, na.rm = T)),
                                ylim = c(min(site_list$lat, na.rm = T),
                                         max(site_list$lat, na.rm = T)),
                         plot = F, interior = F, resolution = 0, lforce = "e")

  # Interpolate for finer resolution
  if(length(coastline$x) < length(site_list$lon)){
    lon2 <- akima::aspline(coastline$x, n = length(site_list$lon)*5)$y
  } else {
    lon2 <- akima::aspline(coastline$x, n = length(site_list$lon)*2)$y
  }
  if(length(coastline$y) < length(site_list$lat)){
    lat2 <- akima::aspline(coastline$y, n = length(site_list$lat)*5)$y
  } else {
    lat2 <- akima::aspline(coastline$y, n = length(site_list$lat)*2)$y
  }

  # Test visuals
  # ggplot() +
  #   geom_point(aes(x = coastline$x, y = coastline$y))

  # ggplot() +
  #   geom_point(aes(x = lon2, y = lat2)) +
  #   coord_cartesian(xlim = c(18, 19), ylim = c(-35, -33))


  # Create index of site position along coast
  idx <- FNN::knnx.index(coastline2[,1:2],
                               as.matrix(cbind(site_list$lon,
                                               site_list$lat)), k = 1)
  # Add the index to the front of the sites data.frame
  site_list <- cbind(idx, site_list)
  # Order sites by index, west to east
  site_list <- site_list[order(site_list$idx, decreasing = TRUE),]
  # Remove index column
  site_list$idx <- NULL
  # Relabel order of sites to match new sequential order
  site_list$order <- as.integer(1:length(site_list$order))
  # plot(site_list$lon, site_list$lat, pch = 1, cex = 0.3, col = "blue") # just checking --> okay
  # Order index factor for use with other scripts
  # site_list$index <- reorder(site_list$index, site_list$order)
  return(site_list)
}

# Testing
# load("~/SACTNraw/metadata/site_list_v4.1.Rdata")
# site_list <- site_list[17:32,]
# site_list2 <- seq.sites(site_list)
