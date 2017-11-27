#' Meta-data for \emph{in situ} collection sites around South Africa.
#'
#' A dataset containing the meta-data for the sites found within
#' the South African Coastal Temperature Network (SACTN).
#'
#' @format A data frame with 135 rows and 19S variables:
#' \describe{
#'   \item{order}{The order of the sites along the coast}
#'   \item{index}{A unique character vector identifying each site}
#'   \item{site}{The name of the site}
#'   \item{src}{The body that collected the data}
#'   \item{lon}{The longitude of the site}
#'   \item{lat}{The latitude of the site}
#'   \item{depth}{The depthof collection in metres}
#'   \item{type}{The type of instrument used}
#'   \item{coast}{The section of the coastline where this site is found}
#'   \item{date.start}{The first day of sampling}
#'   \item{date.end}{The most recent OR last day of sampling}
#'   \item{length}{The length of the time series in days}
#'   \item{NA.perc}{The percentage of missing (NA) days}
#'   \item{mean}{The overall mean temperature in degrees Celsius}
#'   \item{sd}{The overall standard deviation in degrees Celsius}
#'   \item{range}{The overall range in degrees Celsius}
#'   \item{min}{The minimum temperature in degrees Celsius}
#'   \item{max}{The maximum temperature in degrees Celsius}
#'   \item{DT}{The decadal trend in degrees Celsius per decade}
#' }
#' @source \url{https://github.com/ajsmit/SACTN}
"SACTN_site_list"
