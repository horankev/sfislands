#' Create first-order queen contiguity neighbourhood structure with additional connections when islands are present, ensuring that there are no unconnected units
#'
#' @param df an `sf` or `sfc` object.
#' @param row_identifier name of a column from `df` containing names (or unique identifiers) for each row.
#' @param remove_islands default `FALSE`. Whether or not to omit islands from contiguity construction.
#' @param link_islands_k an integer, k. The number of nearest units to which each island should be connected.
#' @param nb_structure default `"list"`. Can also be `"matrix"`. The format in which to return the named contiguity structure.
#' @param add_to_dataframe default `TRUE`. Whether or not to augment existing df with contiguity output as `"nb"` column. `FALSE` returns only the contiguity structure.
#' @param threshold default 1.001. factor by which to change the size of buffer automatically generated around islands to account for imprecisions which may arise from sf::st_buffer() functionality. This can be increased if connections to islands are not occurring as expected.
#' @param geom_col_name name of a column from `df` containing names (or unique identifiers) for each row. This argument is now deprecated and the new "row_identifier" argument replaces it with the same functionality.
#'
#' @return Either a named neighbourhood list or matrix, or an `sf` dataframe with list or matrix included as `"nb"` column.
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name")
st_bridges <- function(df,
                       row_identifier,
                       remove_islands = FALSE,
                       link_islands_k = 1,
                       nb_structure = "list",
                       add_to_dataframe = TRUE,
                       threshold = 1.001,
                       geom_col_name = lifecycle::deprecated())
{
  if (lifecycle::is_present(geom_col_name)) {
    lifecycle::deprecate_warn("1.1.0", "st_bridges(geom_col_name)", "st_bridges(row_identifier)")
    row_identifier <- geom_col_name
  }

  # Get the geometry column name
  geom_col <- attr(df, "sf_column")

  if (length(unique(df |> dplyr::pull({{ row_identifier }}))) != nrow(df)) {
    stop("Duplicate row identifiers present", call. = FALSE)
  }
  if (remove_islands == TRUE){
    # unconnected units
    cont <-  df |>
      sf::st_intersects()
    still_unconnected <- lengths(cont)
    unconnected <- which(still_unconnected == 1)
    df <- df[-unconnected,]
    link_islands_k = 0
  }
  if (link_islands_k > 0)
  {
    # unconnected units
    cont <-  df |>
      sf::st_intersects()
    still_unconnected <- lengths(cont)
    unconnected <- which(still_unconnected == 1)
    # Calculate distances
    distdf <- data.frame(
      constnumb = 0,
      ndist = 0
    )
    for (i in 1:length(unconnected))
    {
      # Fix: use sf::st_geometry() instead of df$geometry
      distances <- sf::st_distance(sf::st_geometry(df)[unconnected[i]], sf::st_geometry(df)) |>
        as.numeric() |> sort()
      distdf[i,2] <- distances[link_islands_k+1] * threshold
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

    # attempt to use st_is_within_distance instead of buffer and intersects
    # cont <- purrr::map2(seq_len(nrow(df)), bufs, function(i, buf) {
    #   # Apply st_is_within_distance for the ith row and buffer distance buf
    #   sf::st_is_within_distance(df[i, ], df, dist = buf)
    # }) |>
    #   purrr::imap(~setdiff(.x, .y))  # Remove self-neighbor (if any)

    names(cont) <- df |> dplyr::pull({{ row_identifier }})
    class(cont) <- c("nb","list")
  }
  # otherwise, just return unaltered contiguity structure
  if(link_islands_k <= 0)
  {
    cont <- df |>
      sf::st_intersects() |>
      purrr::imap(~setdiff(.x,.y))
    names(cont) <- df |> dplyr::pull({{ row_identifier }})
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
      tempdf[[row_identifier]] <- factor(tempdf[[row_identifier]])
      # Fix: use actual geometry column name instead of hardcoded "geometry"
      cols_to_reposition <- c("nb", geom_col)
      tempdf <- tempdf |>
        dplyr::select(dplyr::everything(),dplyr::all_of(cols_to_reposition))
      return(tempdf)
    }
    if(nb_structure =="matrix"){
      cont2 <- spdep::nb2mat(cont, style="B")
      rownames(cont2) <- names(cont)
      tempdf <- df |>
        dplyr::mutate(nb = cont2)
      tempdf[[row_identifier]] <- factor(tempdf[[row_identifier]])
      # Fix: use actual geometry column name instead of hardcoded "geometry"
      cols_to_reposition <- c("nb", geom_col)
      tempdf <- tempdf |>
        dplyr::select(dplyr::everything(),dplyr::all_of(cols_to_reposition))
      return(tempdf)
    }
  }
}
