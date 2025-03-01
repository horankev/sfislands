#' Enforce contiguity between pairs of areas
#'
#' @param nb a neighbourhood `"list"` or `"matrix"`, or an `sf` dataframe with a neighbourhood column called `"nb"`.
#' @param x name or number of first area (optional if `xy_df` is provided).
#' @param y name or number of second area (optional if `xy_df` is provided).
#' @param xy_df (optional) a dataframe with two columns: `x` and `y`, containing the names or numbers of areas to join.
#'
#' @return An amended neighbourhood `"list"`, `"matrix"`, or `sf` dataframe with a neighbourhood column called `"nb"`.
#' @export
#'
#' @examples
#' # For individual x and y
#' st_bridges(uk_election,"constituency_name") |>
#' st_force_join_nb(x = "Gower", y = "Bridgend")
#'
#' # For multiple x and y pairse") |>
#' st_bridges(uk_election,"constituency_name") |>
#' st_force_join_nb(xy_df = data.frame(
#' x = c("Gower", "Llanelli"),
#' y = c("Bridgend", "Vale Of Glamorgan")))
st_force_join_nb <- function(nb, x = NULL, y = NULL, xy_df = NULL) {

  # Check if nb is valid
  if (!(is.data.frame(nb) || is.list(nb$nb) || is.matrix(nb$nb) || is.list(nb) || is.matrix(nb))) {
    stop("Error: The 'nb' argument must be a neighbours list, a neighbours matrix, or a dataframe containing a neighbours list or matrix named 'nb'")
  } else if (is.data.frame(nb) && !("nb" %in% colnames(nb))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }

  # Determine number of areas (length of neighbourhood list/matrix)
  nb_length <- if (is.data.frame(nb)) {
    if (is.list(nb$nb)) {
      length(nb$nb)
    } else if (is.matrix(nb$nb)) {
      nrow(nb$nb)
    } else {
      stop("Error: Invalid neighbourhood structure in dataframe.")
    }
  } else if (is.list(nb)) {
    length(nb)
  } else if (is.matrix(nb)) {
    nrow(nb)
  } else {
    stop("Error: Unrecognised neighbourhood format.")
  }

  # Helper to check whether a pair (x, y) is valid
  check_pair_validity <- function(x, y) {
    if (is.character(x) && is.character(y)) {
      # When x and y are names, check against valid names
      valid_names <- if (is.data.frame(nb)) {
        if (is.list(nb$nb)) {
          names(nb$nb)
        } else if (is.matrix(nb$nb)) {
          rownames(nb$nb)
        } else {
          stop("Error: Invalid neighbourhood structure in dataframe.")
        }
      } else if (is.list(nb)) {
        names(nb)
      } else if (is.matrix(nb)) {
        rownames(nb)
      } else {
        stop("Error: Unrecognised neighbourhood format.")
      }

      if (!(x %in% valid_names)) {
        stop(paste0("Error: x = '", x, "' is not found in the neighbourhood structure."))
      }
      if (!(y %in% valid_names)) {
        stop(paste0("Error: y = '", y, "' is not found in the neighbourhood structure."))
      }

    } else if (is.numeric(x) && is.numeric(y)) {
      # When x and y are numeric indices
      if (x < 1 || x > nb_length || y < 1 || y > nb_length) {
        stop(paste0("Error: x or y index is out of range. Valid indices are between 1 and ", nb_length, "."))
      }
    } else {
      stop("Error: x and y must either both be numeric or both be character (names).")
    }
  }

  # Process pairs from xy_df (if provided)
  if (!is.null(xy_df)) {
    if (!all(c("x", "y") %in% colnames(xy_df))) {
      stop("Error: xy_df must contain columns 'x' and 'y'")
    }

    for (i in seq_len(nrow(xy_df))) {
      check_pair_validity(xy_df$x[i], xy_df$y[i])
      nb <- process_pair(nb, xy_df$x[i], xy_df$y[i])
    }
    return(nb)
  }

  # Process single x, y pair (if xy_df not provided)
  if (is.null(x) || is.null(y)) {
    stop("Error: Either provide 'x' and 'y', or provide 'xy_df'.")
  }

  check_pair_validity(x, y)
  nb <- process_pair(nb, x, y)

  return(nb)
}

# Helper function to process a single pair of x and y
process_pair <- function(nb, x, y) {

  # Convert names to numeric indices if needed
  if (is.character(x) && is.character(y)) {
    if (is.data.frame(nb)) {
      if (is.list(nb$nb)) {
        xnum <- which(names(nb$nb) == x)
        ynum <- which(names(nb$nb) == y)
      } else if (is.matrix(nb$nb)) {
        xnum <- which(rownames(nb$nb) == x)
        ynum <- which(rownames(nb$nb) == y)
      }
    } else if (is.list(nb)) {
      xnum <- which(names(nb) == x)
      ynum <- which(names(nb) == y)
    } else if (is.matrix(nb)) {
      xnum <- which(rownames(nb) == x)
      ynum <- which(rownames(nb) == y)
    }
  } else {
    xnum <- as.integer(x)
    ynum <- as.integer(y)
  }

  # Extract contiguity list
  if (is.data.frame(nb)) {
    tempnb <- nb$nb
    if (is.matrix(tempnb)) {
      tempnb <- spdep::mat2listw(tempnb, style = "B")$neighbours
    }
  } else if (is.matrix(nb)) {
    tempnb <- spdep::mat2listw(nb, style = "B")$neighbours
  } else {
    tempnb <- nb
  }

  # Check if they are already neighbours
  if (!(xnum %in% tempnb[[ynum]])) {
    tempnb2 <- tempnb
    tempnb2[[xnum]] <- sort(c(tempnb[[xnum]], ynum))
    tempnb2[[xnum]] <- tempnb2[[xnum]][tempnb2[[xnum]] != 0]
    tempnb2[[ynum]] <- sort(c(tempnb[[ynum]], xnum))
    tempnb2[[ynum]] <- tempnb2[[ynum]][tempnb2[[ynum]] != 0]
    class(tempnb2) <- c("nb", "list")

    if (is.data.frame(nb)) {
      if (is.matrix(nb$nb)) {
        nb$nb <- spdep::nb2mat(tempnb2, style = "B")
      } else {
        nb$nb <- tempnb2
      }
    } else if (is.matrix(nb)) {
      nb <- spdep::nb2mat(tempnb2, style = "B")
    } else {
      nb <- tempnb2
    }
  }

  return(nb)
}
