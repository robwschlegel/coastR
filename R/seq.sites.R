#' Sequential coastal site order
#'
#' This function finds the order of sites along a convoluted but
#' contiguous coastline. It is not designed to work for multiple
#' different coastlines at once.
#' @param site_list A dataframe with a 'lon' and 'lat' column that
#' contains coordinates along a coastline that need to be ordered
#' @param coast An optional set of coastal coordinates supplied by
#' the user.
#' @keywords sequential sites
#' @export
#' @examples
#' load("data/site_list_v4.1.Rdata")
#' site_list <- site_list[17:32,]
#' site_list2 <- seq.sites(site_list)



seq.sites <- function(site_list, coast = NULL){

  # Create site list

  # Check for lon/lat columns
  if(is.null(site_list$lon)) return("Please ensure a 'lon' column is provided.")
  if(is.null(site_list$lat)) return("Please ensure a 'lat' column is provided.")

  # Fetch coastline values
  coastline <- maps::map(xlim = c(min(site_list$lon, na.rm = T),
                                  max(site_list$lon, na.rm = T)),
                         ylim = c(min(site_list$lat, na.rm = T),
                                  max(site_list$lat, na.rm = T)),
                         plot = F, interior = F,
                         resolution = 0, lforce = "e")

  # Extract coastal coordinates
  lon2 <- coastline$x
  lat2 <- coastline$y

  # Interpolate for finer resolution
  while(length(lon2) < length(site_list$lon)){
    lon2 <- akima::aspline(lon2, n = length(site_list$lon)*5)$y
  }
  while(length(lat2) < length(site_list$lat)){
    lat2 <- akima::aspline(lat2, n = length(site_list$lat)*5)$y
  }

  # Test visuals
  # ggplot() +
  #   geom_point(aes(x = coastline$x, y = coastline$y))
  # ggplot() +
  #   geom_point(aes(x = lon2, y = lat2)) +
  #   coord_cartesian(xlim = c(18, 19), ylim = c(-35, -33))

  # Create index of site position along coast
  idx <- FNN::knnx.index(na.omit(as.matrix(cbind(lon2, lat2))),
                               as.matrix(cbind(site_list$lon,
                                               site_list$lat)), k = 1)
  # Add the index to the front of the sites data.frame
  site_list <- cbind(idx, site_list)
  # Order sites by index, west to east
  site_list <- site_list[order(site_list$idx, decreasing = TRUE),]
  # Remove index column
  site_list$idx <- NULL
  # Relabel order of sites to match new sequential order
  if(!(is.null(site_list$order))) warning("The 'order' column has been overwritten.")
  site_list$order <- as.integer(1:length(site_list$order))
  # Order index factor for use with other scripts
  # site_list$index <- reorder(site_list$index, site_list$order)
  return(site_list)
}

# Testing
# load("data/site_list_v4.1.Rdata")
# site_list <- site_list[17:32,]
# site_list2 <- seq.sites(site_list)

## TO DO
# Add testing for Gulf of Mexico
# Add testing for the Baja Peninsula
# Add testing for Alaskan Peninsula
# Add testing for Yellow Sea

