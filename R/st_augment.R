#' Augment dataframe with predictions of model
#'
#' @param model
#' @param df
#'
#' @return
#' @export
#'
#' @examples
st_augment <- function(model,df){

  if (!(class(model)[1] %in% "gam")) {
    stop("Error: This function requires the model to be of class 'gam'")
  }

  # Identify numeric columns
  numeric_columns <- sapply(df, is.numeric)

  # Replace numeric columns with the number 1
  tempdf <- df
  tempdf[, numeric_columns] <- 1

  output <- stats::predict(model, tempdf, type = "terms", se.fit = TRUE) |>
    as.data.frame()

  ### need different process for renaming cols if one versus more than one smooth:

  ### if only one smooth:

  if(ncol(output) == 2 & nrow(summary(model$smooth)) == 1) {
    # change names from fit. to the type of effect (random effect or mrf.smooth)
    names(output)[1] <- paste0(summary(model$smooth)[,2],".")
    names(output)[stringr::str_starts(names(output),"fit.s.")] <- stringr::str_replace(names(output)[stringr::str_starts(names(output),"fit.s.")],
                                                                                       "fit.s.",
                                                                                       paste0(summary(model$smooth)[,2],"."))
    # same for standard error columns
    names(output)[2] <- paste0("se.",summary(model$smooth)[,2],".")
  }

  ### if more than one smooth:

  else {

    # change names from fit. to the type of effect (random effect or mrf.smooth)
    names(output)[stringr::str_starts(names(output),"fit.s.")] <- stringr::str_replace(names(output)[stringr::str_starts(names(output),"fit.s.")],
                                                                                       "fit.s.",
                                                                                       paste0(summary(model$smooth)[,2],"."))
    # same for standard error columns
    names(output)[stringr::str_starts(names(output),"se.fit.s.")] <- stringr::str_replace(names(output)[stringr::str_starts(names(output),"se.fit.s.")],
                                                                                          "se.fit.s.",
                                                                                          paste0("se.",summary(model$smooth)[,2],"."))
  }

  # remove the . at the end of each matching string
  names(output) <- stringr::str_remove_all(names(output), "\\.$")

  # swap around and put a | in the mrf smooths
  names(output) <- stringr::str_replace_all(names(output), "\\.{2}", "|")

  # rearrange the random.effect colnames
  names(output) <- stringr::str_replace_all(names(output), "random\\.effect\\.(.*?)\\.", "random.effect.\\1|")

  # swap order around the | character
  names(output) <- stringr::str_replace_all(names(output), "\\.([^.]*)\\|(.*)", ".\\2|\\1")

  output2 <- cbind(output,df) |>
    as.data.frame() |>
    dplyr::select(-dplyr::matches("fit\\.")) |>
    dplyr::select(-(1:ncol(output)), dplyr::everything()) |>
    sf::st_as_sf()

  return(output2)

}
