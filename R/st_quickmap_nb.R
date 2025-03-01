#' Visualise a neighbourhood structure on a map
#'
#' @param nbsf an `sf` dataframe with a neighbourhood column called "nb", such as the output of `st_bridges()`
#' @param linkcol colour of lines connecting neighbours.
#' @param bordercol colour of boundary lines between areas.
#' @param pointcol colour of centroid points if nodes are `"point"`.
#' @param fillcol fill of areas.
#' @param linksize linewidth of lines connecting neighbours.
#' @param bordersize linewidth of borders between areas.
#' @param pointsize size of centroid points if nodes are `"point"`.
#' @param title plot title.
#' @param subtitle plot subtitle.
#' @param nodes default `"point"`. Can also be `"numeric"`.
#' @param numericsize font size if nodes are `"numeric"`.
#' @param numericcol font colour if nodes are `"numeric"`.
#' @param concavehull default `FALSE`. Whether or not to show concave hulls.
#' @param hullratio value between 0 and 1. 1 returns the convex hulls, 0 maximally concave hulls.
#' @param hullcol colour of concave hull lines.
#' @param hullsize line width of concave hull lines.
#'
#' @return A `ggplot` showing areas and neighbourhood structure.
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name") |>
#' st_quickmap_nb()
st_quickmap_nb <- function(nbsf,
                           linkcol = "dodgerblue",
                           bordercol = "gray7",
                           pointcol = "darkred",
                           fillcol = "gray95",
                           linksize = 0.2,
                           bordersize = 0.1,
                           pointsize = 0.8,
                           title = NULL,
                           subtitle = NULL,
                           nodes = "point",
                           numericsize = 5,
                           numericcol = "black",
                           concavehull = FALSE,
                           hullratio = 0.8,
                           hullcol = "darkgreen",
                           hullsize = 0.5) {

  if (!inherits(nbsf, "sf")) {
    stop("Error: This function requires a simple features dataframe as input")
  }
  if (!("nb" %in% colnames(nbsf))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }
  if (!(is.list(nbsf$nb) || is.matrix(nbsf$nb))) {
    stop("Error: The 'nb' argument must be a neighbours list or a neighbours matrix")
  }

  # Convert to spatial format
  df_sp <- sf::as_Spatial(nbsf)
  cont <- if (is.matrix(nbsf$nb)) {
    spdep::mat2listw(nbsf$nb, style = "B")$neighbours
  } else {
    nbsf$nb
  }
  class(cont) <- c("nb", "list")

  # Generate neighbour lines
  neighbors_sf <- methods::as(spdep::nb2lines(cont, coords = df_sp), 'sf')

  if (is.na(sf::st_crs(neighbors_sf))) {
    neighbors_sf <- sf::st_set_crs(neighbors_sf, sf::st_crs(nbsf))
  }

  # Get endpoints
  endpoints_coords <- sf::st_coordinates(neighbors_sf) |> data.frame() |>
    sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(neighbors_sf))

  # Create base plot
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nbsf, fill = fillcol, colour = bordercol, linewidth = bordersize) +
    ggplot2::geom_sf(data = neighbors_sf, colour = linkcol, linewidth = linksize) +
    ggplot2::coord_sf(datum = NA,
                      default = TRUE) + #to deal with unwanted 'coordinate system already present' message
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank())

  # Add numeric labels if nodes = "numeric"
  if (nodes == "numeric") {
    nbsf$id <- seq_len(nrow(nbsf))
    sf::st_agr(nbsf) <- "constant" #explicitly make attribute constant despite the following geometry operations to avoid warnings
    centroids <- sf::st_centroid(nbsf)
    centroids$id <- nbsf$id
    plot <- plot + ggplot2::geom_sf_text(data = centroids,
                                         ggplot2::aes(label = nbsf$id),
                                         size = numericsize, colour = numericcol, fontface = "bold")
  } else {
    plot <- plot + ggplot2::geom_sf(data = endpoints_coords, size = pointsize, colour = pointcol)
  }

  # Add concave hull if enabled
  if (concavehull) {
    plot <- plot + ggplot2::geom_sf(data = sf::st_concave_hull(nbsf, ratio = hullratio),
                                    fill = NA, colour = hullcol, linewidth = hullsize)
  }

  return(plot)
}
