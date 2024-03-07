#' Augment dataframe with predictions of model
#'
#' @param model an `mgcv` or `lme4` model
#' @param df the sf data frame to be augmented with model preditions
#'
#' @return an augmented sd dataframe
#' @export
#'
#' @examples
#' package_name <- "mgcv"
#' if (!require(package_name, character.only = TRUE)) {
#' install.packages(package_name)
#' library(package_name, character.only = TRUE)
#' }
#' prepdata <- st_bridges(uk_election,"constituency_name")
#' mgcv::gam(health_not_good ~
#'   s(constituency_name, bs='mrf', xt=list(nb=prepdata$nb), k=100),data=prepdata, method="REML") |>
#' st_augment(uk_election)

st_augment <- function(model,df){
  UseMethod("st_augment")
}


#' @export
st_augment.default <- function(model,df){

  warning(paste("st_augment does not know how to handle object of class ",
                class(model),
                "and can only be used on classes gam and lmerMod"))

}

#' @export
st_augment.gam <- function(model,df){

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

#' @export
st_augment.lmerMod <- function(model,df) {

  temp1 <- broom.mixed::tidy(model, effects = "ran_vals", conf.int = TRUE) |>
    dplyr::select(2:6)

  # Create a unique identifier for each combination of group and term
  temp1$group_term <- paste(temp1$group, temp1$term, sep = ".")

  # Split dataframe into a list of dataframes
  # based on the unique combinations of group and term
  temp_list <- split(temp1, temp1$group_term)

  # function to change one col name for joining purposes
  # and give desired name structure to estimates and std errors
  myfunct <- function(x) {
    tempname <- unique(x[,1])
    colnames(x)[2] <- as.character(tempname)

    newname1 <- paste0("random.effect.",x$term,"|",x$group)
    newname1_clean <- stringr::str_replace_all(newname1, "\\(Intercept\\)\\|", "")
    names(x)[names(x)=="estimate"] <- newname1_clean

    newname2 <- paste0("se.random.effect.",x$term,"|",x$group)
    newname2_clean <- stringr::str_replace_all(newname2, "\\(Intercept\\)\\|", "")
    names(x)[names(x)=="std.error"] <- newname2_clean

    return(df)
  }

  temp2 <- lapply(temp_list, myfunct)

  # function to join each nested df to original sf df
  # first remove geometry for rejoining later
  # remove the three cols before merging which would lead to NAs
  # due to missing values
  cols_to_remove <- c("group","term","group_term")
  left_join_to_df <- function(y) {
    dplyr::left_join(df |> sf::st_drop_geometry(), df, by=names(y)[2])  |>
      dplyr::select(-cols_to_remove)
  }

  temp3 <- lapply(temp2, left_join_to_df)

  # make list of dfs into one df and add geometry column back
  temp4 <- Reduce(function(x, y) merge(x, y, all=TRUE), temp3) |>
    dplyr::mutate(geometry = df$geometry) |>
    sf::st_as_sf()

  return(temp4)
}
