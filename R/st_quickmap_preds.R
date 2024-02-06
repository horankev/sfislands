#' Visualilse the predictions of an `mgcv` model
#'
#' @param output an augmented sf dataframe produced by st_augment()
#'
#' @return a list of ggplots
#' @export
#'
#' @examples
#' prepdata <- st_bridges(uk_election,"constituency_name")
#' mgcv::gam(health_not_good ~
#'   s(constituency_name, bs='mrf', xt=list(nb=prepdata$nb), k=100),data=prepdata, method="REML") |>
#' st_augment(uk_election) |>
#' st_quickmap_preds()
st_quickmap_preds <- function(output){

  if (!inherits(output,"sf")) {
    stop("Error: This function requires a simple features dataframe as input")
  }

  output1 <- output |>
    dplyr::select(dplyr::starts_with("random.effect"),dplyr::starts_with("mrf.smooth"))

  fillnames <- output1 |>
    sf::st_drop_geometry() |>
    names()

  # split column names into title and subtitle
  # either side of second . in string
  newtitle <- sub("^(.*?\\..*?)\\..*$", "\\1", fillnames)
  # extract the text after random.effect. or mrf.smooth.
  newsubtitle <- stringr::str_replace_all(fillnames, "(random\\.effect\\.|mrf\\.smooth\\.)", "")

  plot_list <- list()
  for (i in 1:length(fillnames)){
    plot_list[[i]] <- ggplot2::ggplot() +
      ggplot2::geom_sf(data=output1, ggplot2::aes(fill=!!as.name(fillnames[i])), linewidth=0.05, colour="black") +
      ggplot2::scale_fill_gradient2() +
      ggplot2::labs(title=newtitle[i],
                    subtitle=newsubtitle[i]) +
      ggplot2::coord_sf(datum=NA) +
      ggplot2::theme_minimal() +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size=10))

  }

  return(plot_list)
}
