#' Sequential coastal site order
#'
#' This function finds the order of sites along a convoluted but
#' contiguous coastline. It is not designed to work for multiple
#' different coastlines at once.
#'
#' @importFrom magrittr %>%
#' @param site_list A dataframe with a 'lon' and 'lat' column that
#' contains coordinates along a coastline that need to be ordered.
#' @param reverse A boolean TRUE/FALSE to determine if the ordering
#' of the sites should be reversed on output. Default = FALSE.
#' @param coast An optional set of coastal coordinates supplied by
#' the user. Default = NULL. Currently not supported...
#' @return A dataframe with the correct ordering of the input sites
#' @keywords sequential sites
#' @export
#' @examples
#' SACTN_site_list_sub <- SACTN_site_list[17:32,]
#' SACTN_site_list_ordered <- seq_sites(SACTN_site_list_sub)

seq_sites <- function(site_list, reverse = FALSE, coast = TRUE){

  # Check for lon/lat columns
  if(is.null(site_list$lon)) return("Please ensure a 'lon' column is provided.")
  if(is.null(site_list$lat)) return("Please ensure a 'lat' column is provided.")

  # Give a warning message if an order column exists in 'site_list'
    # as it will be overwritten
  if(!(is.null(site_list$order))) warning("The 'order' column has been overwritten.")

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

  # Test visuals
  # ggplot() +
  #   geom_point(aes(x = coastline$long, y = coastline$lat))
  # ggplot() +
  #   geom_path(aes(x = coastline$long, y = coastline$lat))
  # ggplot() +
  #   geom_point(aes(x = lon2, y = lat2)) +
  #   coord_cartesian(xlim = c(18, 19), ylim = c(-35, -33))

  # Extract coastal coordinates
  lon2 <- akima::aspline(coastline$long, n = length(coastline$long)*10)$y
  lat2 <- akima::aspline(coastline$lat, n = length(coastline$lat)*10)$y

  # Interpolate for finer resolution if required
  while(length(lon2) < length(site_list$lon)){
    lon2 <- akima::aspline(lon2, n = length(site_list$lon)*5)$y
  }
  while(length(lat2) < length(site_list$lat)){
    lat2 <- akima::aspline(lat2, n = length(site_list$lat)*5)$y
  }

  # Create index of site position along coast
  idx <- base::as.vector(FNN::knnx.index(stats::na.omit(as.matrix(base::cbind(lon2, lat2))),
                                         base::as.matrix(base::cbind(site_list$lon,
                                                                     site_list$lat)), k = 1))
  idx_length <- base::length(idx)
  idx_order <- base::as.integer(1:idx_length)

  # Add the index and order
  site_list <- site_list %>%
    dplyr::mutate(idx = idx) %>%
    dplyr::arrange(dplyr::desc(idx)) %>%
    dplyr::mutate(order = idx_order) %>%
    dplyr::select(order, dplyr::everything(), -idx)

  if(reverse){
    site_list <- site_list %>%
      dplyr::arrange(dplyr::desc(order)) %>%
      dplyr::mutate(order = idx_order)
  }

  # Finish
  return(site_list)
}
