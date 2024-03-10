#' Creates first-order queen contiguity neighbourhood structure with additional connections when islands are present, ensuring that there are no unconnected units
#'
#' @param df an `sf` or `sfc` object.
#' @param geom_col_name name of a column from `df` containing names (or unique identifiers) for each row.
#' @param remove_islands default `FALSE`. Whether or not to omit islands from contiguity construction.
#' @param link_islands_k an integer, k. The number of nearest units to which each island should be connected.
#' @param nb_structure default `"list"`. Can also be `"matrix"`. The format in which to return the named contiguity structure.
#' @param add_to_dataframe default `TRUE`. Whether or not to augment existing df with contiguity output as `"nb"` column. `FALSE` returns only the contiguity structure.
#'
#' @return Either a named neighbourhood list or matrix, or an `sf` dataframe with list or matrix included as `"nb"` column.
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name")
st_bridges <- function(df,
                       geom_col_name,
                       remove_islands = FALSE,
                       link_islands_k = 1,
                       nb_structure = "list",
                       add_to_dataframe = TRUE)
{

  if (remove_islands == TRUE){
    # unconnected units
    cont <-  df |>
      sf::st_intersects()
    still_unconnected <- lengths(lapply(cont, function(x) x))
    unconnected <- which(still_unconnected == 1)

    df <- df[-unconnected,]
    link_islands_k = 0

  }

  if (link_islands_k > 0)
  {
    # unconnected units
    cont <-  df |>
      sf::st_intersects()
    still_unconnected <- lengths(lapply(cont, function(x) x))
    unconnected <- which(still_unconnected == 1)
    # Calculate distances
    distdf <- data.frame(
      constnumb = 0,
      ndist = 0
    )
    for (i in 1:length(unconnected))
    {
      distances <- sf::st_distance(df$geometry[unconnected[i]], sf::st_geometry(df$geometry)) |>
        as.numeric() |> sort()
      distdf[i,2] <- distances[link_islands_k+1] * 1.001
      distdf[i,1] <- unconnected[i]
    }
    bufs <- rep(0,nrow(df))
    for (i in 1:length(unconnected))
    {
      bufs[distdf[i,1]] <- distdf[i,2]
    }

    cont <- df |>
      sf::st_buffer(dist=bufs) |>
      sf::st_intersects() |>
      purrr::imap(~setdiff(.x,.y))

    names(cont) <- df |> dplyr::pull({{ geom_col_name }})

    class(cont) <- c("nb","list")
  }

  # otherwise, just return unaltered contiguity structure
  if(link_islands_k <= 0)
  {
    cont <- df |>
      sf::st_intersects() |>
      purrr::imap(~setdiff(.x,.y))

    names(cont) <- df |> dplyr::pull({{ geom_col_name }})

    class(cont) <- c("nb","list")
  }

  if(add_to_dataframe == FALSE){
    if(nb_structure == "list"){
      return(cont)
    }
    if(nb_structure =="matrix"){
      cont2 <- spdep::nb2mat(cont, style="B")
      rownames(cont2) <- names(cont)
      return(cont2)
    }
  }

  if(add_to_dataframe == TRUE){
    if(nb_structure == "list"){
      tempdf <- df |>
        dplyr::mutate(nb = cont)
      tempdf[[geom_col_name]] <- factor(tempdf[[geom_col_name]])
      return(tempdf)
    }
    if(nb_structure =="matrix"){
      cont2 <- spdep::nb2mat(cont, style="B")
      rownames(cont2) <- names(cont)
      tempdf <- df |>
        dplyr::mutate(nb = cont2)
      tempdf[[geom_col_name]] <- factor(tempdf[[geom_col_name]])
      return(tempdf)
    }
  }
}
