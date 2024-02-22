#' Visualilse the predictions of an `mgcv` model
#'
#' @param output an augmented sf dataframe produced by st_augment()
#' @param scale_low fill of lowest extreme of scale
#' @param scale_mid fill of midpoint of scale
#' @param scale_high fill of highest extreme of scale
#' @param scale_midpoint value of midpoint of scale
#' @param borderwidth linewidth of borders between units
#' @param bordercol colour of borders between units
#' @param limits "individual" (default) each plot scaled to individual min-max. "minmax" means all plot have a common min-max according to the global min-max
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
st_quickmap_preds <- function(output,
                              scale_low = "firebrick4",
                              scale_mid = "white",
                              scale_high = "darkblue",
                              scale_midpoint = 0,
                              borderwidth = 0.05,
                              bordercol = "black",
                              legendlimits = "individual"){

  if (!inherits(output,"sf")) {
    stop("Error: This function requires a simple features dataframe as input")
  }

  output1 <- output |>
    dplyr::select(dplyr::starts_with("random.effect"),dplyr::starts_with("mrf.smooth"))

  min_scale <- output1 |>
    sf::st_drop_geometry() |>
    min(na.rm = TRUE)

  max_scale <- output1 |>
    sf::st_drop_geometry() |>
    max(na.rm = TRUE)

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
      ggplot2::geom_sf(data=output1, ggplot2::aes(fill=!!as.name(fillnames[i])), linewidth=borderwidth, colour=bordercol) +
      ggplot2::scale_fill_gradient2(low = scale_low,
                                    mid = scale_mid,
                                    high = scale_high,
                                    midpoint = scale_midpoint,
                                    limits = ifelse(legendlimits == "minmax",
                                                    c(min_scale, max_scale),
                                                    NULL)) +
      ggplot2::labs(title=newtitle[i],
                    subtitle=newsubtitle[i]) +
      ggplot2::coord_sf(datum=NA) +
      ggplot2::theme_minimal() +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size=10))

  }

  return(plot_list)
}
