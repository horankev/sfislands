#' Create contiguityies when islands are present
#'
#' @param df
#' @param geom_col_name
#' @param remove_islands
#' @param link_islands_k
#' @param nb_structure
#' @param add_to_dataframe
#'
#' @return
#' @export
#'
#' @examples
st_bridges <- function(df, # sf dataframe
                       geom_col_name,
                       remove_islands = FALSE,
                       link_islands_k = 1, # link island to k nearest units, 0 removes all islands
                       nb_structure = "list",
                       add_to_dataframe = TRUE)
{

  if (!inherits(df,"sf")) {
    stop("Error: This function requires a simple features dataframe as input")
  }

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
