#' Visualise a neighbourhood structure on a map
#'
#' @param nbsf an sf dataframe with a neighbourhood column called "nb"
#' @param linkcol colour of lines connecting neighbours
#' @param bordercol colour of boundary lines between areas
#' @param pointcol colour of centroid points if nodes are point
#' @param fillcol fill of areas
#' @param linksize linewidth of lines connecting neighbours
#' @param bordersize linewidth of borders between areas
#' @param pointsize size of centroid points if nodes are point
#' @param title plot title
#' @param subtitle plot subtitle
#' @param nodes point or numeric
#' @param numericsize font size if nodes are numeric
#' @param numericcol font colour if nodes are numeric
#'
#' @return ggplot of areas and neighbourhood structure
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name") |>
#' st_quickmap_nb()
st_quickmap_nb <- function(nbsf,
                           linkcol = "dodgerblue",
                           bordercol = "gray7",
                           pointcol="darkred",
                           fillcol = "gray95",
                           linksize=0.2,
                           bordersize=0.1,
                           pointsize=0.8,
                           title=NULL,
                           subtitle=NULL,
                           nodes="point",
                           numericsize=1,
                           numericcol="black"){

  if (!inherits(nbsf,"sf")) {
    stop("Error: This function requires a simple features dataframe as input")
  }

  if (is.data.frame(nbsf) && !("nb" %in% colnames(nbsf))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }

  if (!(is.list(nbsf$nb) || is.matrix(nbsf$nb))) {
    stop("Error: The 'nb' argument must be a neighbours list or a neighbours matrix")
  }


  # to show the contiguities on a map
  ###
  # first, the dataframe must be a spdf, spatial dataframe
  df_sp <- sf::as_Spatial(nbsf)

  if(is.matrix(nbsf$nb)){
    temp <- spdep::mat2listw(nbsf$nb, style="B")
    cont <- temp[2]
    cont <- cont$neighbours
    class(cont) <- c("nb","list")
  }

  if(is.list(nbsf$nb)){
    cont <- nbsf$nb
    class(cont) <- c("nb","list")
  }
  # make lines where there are contiguities
  neighbors_sf <- methods::as(spdep::nb2lines(cont, coords = df_sp), 'sf')
  neighbors_sf <- sf::st_set_crs(neighbors_sf, sf::st_crs(nbsf))

  # get the endpoints of these lines (they are not necessarily the centroids...)
  endpoints_coords <- sf::st_coordinates(neighbors_sf) |> data.frame() |>
    sf::st_as_sf(coords=c("X","Y"), crs=sf::st_crs(neighbors_sf))

  if(nodes == "numeric"){

    endpoints_coords$id <- 1:nrow(endpoints_coords)

    # map the connections
    ggplot2::ggplot() +
      ggplot2::geom_sf(data=nbsf, fill=fillcol, colour=bordercol, linewidth=bordersize) +
      ggplot2::geom_sf(data = neighbors_sf, colour=linkcol, linewidth=linksize) +
      ggplot2::geom_sf_text(data=endpoints_coords, aes(label=endpoints_coords$id), numericsize=numericsize, numericcol=numericcol) +
      ggplot2::coord_sf(datum=NA) +
      ggplot2::labs(title = title,
                    subtitle = subtitle) +
      ggplot2::theme_void() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

  else{

    # map the connections
    ggplot2::ggplot() +
      ggplot2::geom_sf(data=nbsf, fill=fillcol, colour=bordercol, linewidth=bordersize) +
      ggplot2::geom_sf(data = neighbors_sf, colour=linkcol, linewidth=linksize) +
      ggplot2::geom_sf(data=endpoints_coords, size=pointsize, colour=pointcol) +
      ggplot2::coord_sf(datum=NA) +
      ggplot2::labs(title = title,
                    subtitle = subtitle) +
      ggplot2::theme_void() +
      ggplot2::theme(axis.title.x = ggplot2::element_blank()) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
  }

}
