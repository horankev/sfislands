#' Examine contiguity actions which have been performed on islands by `st_bridges()`
#'
#' @param data an `sf` dataframe with a neighbourhood column called `"nb"` such as the output of `st_bridges()`.
#'
#' @return a dataframe reporting non-contiguous connections made by `st_bridges()`.
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name") |>
#' st_check_islands()
st_check_islands <- function(data){

  if (is.data.frame(data) && !("nb" %in% colnames(data))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }

  if (!(is.list(data$nb) || is.matrix(data$nb))) {
    stop("Error: The 'nb' argument must be a neighbours list or a neighbours matrix")
  }

  # make it a list if it's a matrix
  if(is.matrix(data$nb)){ # matrix within dataframe
    keep_rownames <- rownames(data$nb)
    tempnb <- spdep::mat2listw(data$nb, style="B")
    tempnb <- tempnb[2]
    data$nb <- tempnb$neighbours
    names(data$nb) <- keep_rownames
  }

  # unconnected units
  cont <-  data |>
    sf::st_intersects()
  still_unconnected <- lengths(lapply(cont, function(x) x))
  unconnected <- which(still_unconnected == 1)


  output <- data[unconnected,]$nb
  names(output) <- paste(names(output)," : ",unconnected)

  output2 <- do.call(rbind, lapply(names(output), function(x) cbind(x, output[[x]]))) |>
    data.frame()
  names(output2) <- c("region","nb_num")

  consnames <- names(data$nb)
  output2$nb_names <- consnames[as.integer(output2$nb_num)]

  output2 <- tidyr::separate(output2, "region", into = c("island_names", "island_num"), sep = "  : ")

  return(output2)
}
