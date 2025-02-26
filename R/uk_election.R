#' UK election data
#'
#' Swing and socio-economic data for England, Scotland & Wales
#' Census and voting data sourced from parlitools R package
#' Spatial data sourced from UK government geoportal
#'
#' @format ## `uk_election`
#'    An sf and data.frame object with 632 rows and 9 columns
#'
#' \describe{
#'   \item{degree_educated}{Percentage of constituency population with level 4 qualifications or higher, scaled to mean 0 and standard deviation 1}
#'   \item{health_not_good}{Percentage of constituency of population reporting health to be fair, bad, or very bad, scaled to mean 0 and standard deviation 1}
#'   \item{white}{Percentage of constituency of population of exclusively white ethnicity, scaled to mean 0 and standard deviation 1}
#'   \item{con_swing}{Butler swing to the Conservative Party from the Labour Party from election 2019 to election 2019}
#'   \item{population}{Constituency population}
#'   \item{region}{Regions}
#'   \item{county}{Counties}
#'   \item{constituency_name}{Westminster parliamentary constituencies, as of 2019}
#'   \item{geometry}{sfc polygons column}
#'   ...
#' }
#'
#' @source <https://geoportal.statistics.gov.uk/datasets/ons::wpc-dec-2019-ultra-generalised-clipped-boundaries-uk>, <https://docs.evanodell.com/parlitools/>
"uk_election"
