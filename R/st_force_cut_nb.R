#' Remove contiguity between pairs of areas
#'
#' @param nb a neighbourhood `"list"` or `"matrix"`, or an `sf` dataframe with a neighbourhood column called `"nb"`.
#' @param x name or number of first area (optional if `xy_df` is provided).
#' @param y name or number of second area (optional if `xy_df` is provided).
#' @param xy_df (optional) a dataframe with two columns: `x` and `y`, containing the names or numbers of areas to cut.
#'
#' @return An amended neighbourhood `"list"`, `"matrix"`, or `sf` dataframe with a neighbourhood column called `"nb"`.
#' @export
#'
#' @examples
#' # For individual x and y
#' st_bridges(uk_election,"constituency_name") |>
#' st_force_cut_nb(x = "Llanelli", y = "Swansea West")
#'
#' # For multiple x and y pairs
#' xy_df <- data.frame(
#' x = c("Llanelli", "Swansea West"),
#' y = c("Bridgend", "Vale Of Glamorgan")
#' )
#' st_bridges(uk_election,"constituency_name") |>
#' st_force_cut_nb(xy_df = xy_df)
st_force_cut_nb <- function(nb, x = NULL, y = NULL, xy_df = NULL) {

  # Check if nb is a valid neighbourhood list, matrix, or dataframe containing 'nb' column
  if (!(is.data.frame(nb) || is.list(nb$nb) || is.matrix(nb$nb) || is.list(nb) || is.matrix(nb))) {
    stop("Error: The 'nb' argument must be a neighbours list, a neighbours matrix, or a dataframe containing a neighbours list or matrix named 'nb'")
  } else if (is.data.frame(nb) && !("nb" %in% colnames(nb))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }

  # If xy_df is provided, process multiple x, y pairs from the dataframe
  if (!is.null(xy_df)) {
    # Ensure that xy_df has x and y columns
    if (!all(c("x", "y") %in% colnames(xy_df))) {
      stop("Error: The dataframe must contain columns 'x' and 'y'")
    }

    # Loop over the rows of xy_df and perform the cut for each pair
    for (i in 1:nrow(xy_df)) {
      x <- xy_df$x[i]
      y <- xy_df$y[i]

      # Process the current pair
      nb <- process_cut_pair(nb, x, y)
    }

    return(nb)
  }

  # If no xy_df, proceed with single x and y processing
  if (is.null(x) || is.null(y)) {
    stop("Error: Either provide 'x' and 'y' or 'xy_df'.")
  }

  # Process the single pair
  nb <- process_cut_pair(nb, x, y)

  return(nb)
}

# Helper function to process a single pair of x and y for cutting
process_cut_pair <- function(nb, x, y) {

  # MAKE INTEGERS: xnum and ynum
  if(is.numeric(x)) { xnum <- as.integer(x) }
  if(is.numeric(y)) { ynum <- as.integer(y) }

  # case when character x and y are provided:
  if(is.character(x) & is.character(y)) {
    if(is.data.frame(nb)) {
      if(is.character(x)) {
        if(is.list(nb$nb)) {
          xnum <- which(names(nb$nb) == x) |> as.integer()
        }
        if(is.matrix(nb$nb)) {
          xnum <- which(rownames(nb$nb) == x) |> as.integer()
        }
      }
      if(is.character(y)) {
        if(is.list(nb$nb)) {
          ynum <- which(names(nb$nb) == y) |> as.integer()
        }
        if(is.matrix(nb$nb)) {
          ynum <- which(rownames(nb$nb) == y) |> as.integer()
        }
      }
    } else {
      if(is.list(nb)) {
        xnum <- which(names(nb) == x) |> as.integer()
        ynum <- which(names(nb) == y) |> as.integer()
      }
      if(is.matrix(nb)) {
        xnum <- which(rownames(nb) == x) |> as.integer()
        ynum <- which(rownames(nb) == y) |> as.integer()
      }
    }
  }

  # Extract the contiguity as list for all forms: tempnb
  if(is.data.frame(nb)) {
    tempnb <- nb$nb # list or matrix within dataframe

    if(is.matrix(tempnb)) { # matrix within dataframe
      tempnb <- spdep::mat2listw(tempnb, style="B")
      tempnb <- tempnb[2]
      tempnb <- tempnb$neighbours
    }
  } else {
    if(is.matrix(nb)) {
      tempnb <- spdep::mat2listw(nb, style="B")
      tempnb <- tempnb[2]
      tempnb <- tempnb$neighbours
    }
    if(is.list(nb)) {
      tempnb <- nb
    }
  }

  # If x and y are not neighbours, return original structure unchanged
  if(!(xnum %in% tempnb[[ynum]])) {
    return(nb)
  } else { # Perform the cutting operation
    tempnb2 <- tempnb
    tempnb2[[xnum]] <- tempnb[[xnum]][tempnb[[xnum]] != ynum] |> sort()
    tempnb2[[ynum]] <- tempnb[[ynum]][tempnb[[ynum]] != xnum] |> sort()

    class(tempnb2) <- c("nb", "list")

    # Return in appropriate form
    if(is.data.frame(nb)) {
      if(is.matrix(nb$nb)) {
        tempmat <- spdep::nb2mat(tempnb2, style="B")
        dfmat_return <- nb
        dfmat_return$nb <- tempmat
        nb <- dfmat_return
      }
      if(is.list(nb$nb)) {
        dflist_return <- nb
        dflist_return$nb <- tempnb2
        nb <- dflist_return
      }
    } else {
      if(is.list(nb)) {
        nb <- tempnb2
      }
      if(is.matrix(nb)) {
        tempmat <- spdep::nb2mat(tempnb2, style="B")
        nb <- tempmat
      }
    }
  }

  return(nb)
}
