#' Sequential coastal site order
#'
#' This function finds the order of sites along a convoluted but
#' contiguous coastline. It is not designed to work for multiple
#' different coastlines at once.
#'
#' @param site_list A dataframe with a 'lon' and 'lat' column that
#' contains coordinates along a coastline that need to be ordered
#' @param coast An optional set of coastal coordinates supplied by
#' the user. Defaults to NULL. Currently not supported...
#' @keywords sequential sites
#' @examples
#' load("data/site_list_v4.1.RData")
#' site_list <- site_list[17:32,]
#' site_list2 <- seq.sites(site_list)
#' @return A dataframe with the correct ordering of the input sites
#' @export

seq.sites <- function(site_list, coast = NULL){

  # Check for lon/lat columns
  if(is.null(site_list$lon)) return("Please ensure a 'lon' column is provided.")
  if(is.null(site_list$lat)) return("Please ensure a 'lat' column is provided.")

  # Give a warning message if an order column exists in 'site_list'
    # as it will be overwritten
  if(!(is.null(site_list$order))) warning("The 'order' column has been overwritten.")

  # Fetch coastline values
  coastline <- maps::map(xlim = c(min(site_list$lon, na.rm = T),
                                  max(site_list$lon, na.rm = T)),
                         ylim = c(min(site_list$lat, na.rm = T),
                                  max(site_list$lat, na.rm = T)),
                         plot = F, interior = F,
                         resolution = 0, lforce = "e")

  # Extract coastal coordinates
  lon2 <- akima::aspline(coastline$x, n = length(coastline$x)*10)$y
  lat2 <- akima::aspline(coastline$y, n = length(coastline$y)*10)$y

  # Interpolate for finer resolution if required
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
  idx <- as.vector(FNN::knnx.index(na.omit(as.matrix(cbind(lon2, lat2))),
                         as.matrix(cbind(site_list$lon,
                                         site_list$lat)), k = 1))

  # Add the index and order
  site_list <- site_list %>%
    mutate(idx = idx) %>%
    arrange(desc(idx)) %>%
    mutate(order = as.integer(1:n())) %>%
    select(order, everything(), -idx)

  # Finish
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

