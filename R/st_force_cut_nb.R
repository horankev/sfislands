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
#' st_bridges(uk_election,"constituency_name") |>
#' st_force_cut_nb(xy_df = data.frame(
#' x = c("Llanelli", "Swansea West"),
#' y = c("Bridgend", "Vale Of Glamorgan")))
st_force_cut_nb <- function(nb, x = NULL, y = NULL, xy_df = NULL) {
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

  # Helper: check validity of x, y pair
  check_pair_validity <- function(x, y) {
    if (is.character(x) && is.character(y)) {
      valid_names <- if (is.data.frame(nb)) {
        if (is.list(nb$nb)) {
          names(nb$nb)
        } else if (is.matrix(nb$nb)) {
          rownames(nb$nb)
        }
      } else if (is.list(nb)) {
        names(nb)
      } else if (is.matrix(nb)) {
        rownames(nb)
      }
      if (!(x %in% valid_names)) stop(paste0("Error: x = '", x, "' not found in neighbourhood."))
      if (!(y %in% valid_names)) stop(paste0("Error: y = '", y, "' not found in neighbourhood."))
    } else if (is.numeric(x) && is.numeric(y)) {
      if (x < 1 || x > nb_length || y < 1 || y > nb_length) {
        stop(paste0("Error: x or y index out of range (1 to ", nb_length, ")."))
      }
    } else {
      stop("Error: x and y must both be numeric (indices) or both character (names).")
    }
  }

  # Process multiple pairs from xy_df
  if (!is.null(xy_df)) {
    if (!all(c("x", "y") %in% colnames(xy_df))) {
      stop("Error: xy_df must contain columns 'x' and 'y'")
    }

    for (i in seq_len(nrow(xy_df))) {
      check_pair_validity(xy_df$x[i], xy_df$y[i])
      nb <- process_cut_pair(nb, xy_df$x[i], xy_df$y[i])
    }
    return(nb)
  }

  # Process single pair
  if (is.null(x) || is.null(y)) {
    stop("Error: Either provide 'x' and 'y', or provide 'xy_df'.")
  }

  check_pair_validity(x, y)
  nb <- process_cut_pair(nb, x, y)

  return(nb)
}

# Helper to convert names to indices (if needed) and cut the link
process_cut_pair <- function(nb, x, y) {
  if (is.character(x) && is.character(y)) {
    if (is.data.frame(nb)) {
      if (is.list(nb$nb)) {
        x <- which(names(nb$nb) == x)
        y <- which(names(nb$nb) == y)
      } else if (is.matrix(nb$nb)) {
        x <- which(rownames(nb$nb) == x)
        y <- which(rownames(nb$nb) == y)
      }
    } else if (is.list(nb)) {
      x <- which(names(nb) == x)
      y <- which(names(nb) == y)
    } else if (is.matrix(nb)) {
      x <- which(rownames(nb) == x)
      y <- which(rownames(nb) == y)
    }
  }

  x <- as.integer(x)
  y <- as.integer(y)

  # Extract list form of nb
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

  # Perform the cut (if they are neighbours)
  if (y %in% tempnb[[x]]) {
    tempnb[[x]] <- setdiff(tempnb[[x]], y)
    tempnb[[y]] <- setdiff(tempnb[[y]], x)
  }

  class(tempnb) <- c("nb", "list")

  if (is.data.frame(nb)) {
    if (is.matrix(nb$nb)) {
      nb$nb <- spdep::nb2mat(tempnb, style = "B")
    } else {
      nb$nb <- tempnb
    }
    return(nb)
  } else if (is.matrix(nb)) {
    return(spdep::nb2mat(tempnb, style = "B"))
  } else {
    return(tempnb)
  }
}
