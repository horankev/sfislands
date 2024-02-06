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
#'   \item{degree_educated}{percent of population with level 4 qualifications or higher}
#'   \item{health_not_good}{percent of population reporting health to be fair, bad, or very bad}
#'   \item{white}{percent of population of exclusively white ethnicity}
#'   \item{con_swing}{Butler swing to the Conservative Party from the Labour Party from election 2019 to election 2019}
#'   \item{population}{Constituency population}
#'   \item{region}{Region}
#'   \item{county}{County}
#'   \item{constituency_name}{Constituency}
#'   \item{geometry}{sfc polygons column}
#'   ...
#' }
#'
#' @source <https://geoportal.statistics.gov.uk/datasets/ons::wpc-dec-2019-ultra-generalised-clipped-boundaries-uk>, <https://docs.evanodell.com/parlitools/>
"uk_election"
