#' Manual remove contiguity between two areas
#'
#' @param nb a neighbourhood `"list"` or `"matrix"`, or an `sf` dataframe with a neighbourhood column called `"nb"`.
#' @param x name or number of first area.
#' @param y name or number of second area.
#'
#' @return an amended neighbourhood `"list"`, `"matrix"`, or `sf` dataframe with a neighbourhood column called `"nb"`.
#' @export
#'
#' @examples
#' st_bridges(uk_election,"constituency_name") |>
#' st_manual_cut_nb("Ynys Mon","Arfon") |>
#' st_manual_cut_nb(292,378)
st_manual_cut_nb <- function(nb,x,y){

  if (!(is.data.frame(nb) || is.list(nb$nb) || is.matrix(nb$nb) || is.list(nb) || is.matrix(nb))) {
    stop("Error: The 'nb' argument must be a neighbours list, a neighbours matrix, or a dataframe containing a neighbours list or matrix named 'nb'")
  } else if (is.data.frame(nb) && !("nb" %in% colnames(nb))) {
    stop("Error: The dataframe must contain a column called 'nb'")
  }

  ### case when numeric x and y:
  # xnum<-x |> as.integer()
  # ynum<-y |> as.integer()
  if(is.numeric(x)){xnum <- x |> as.integer()}
  if(is.numeric(y)){ynum <- y |> as.integer()}

  ### case when character x and y:
  # first, make into numeric is given as character
  # depending on its structure as df, list, matrix...
  if(is.character(x) & is.character(y)){

    if(is.data.frame(nb)){
      if(is.character(x)){
        if(is.list(nb$nb)){
          xnum <- which(names(nb$nb)==x) |>
            as.integer()
        }
        if(is.matrix(nb$nb)){
          xnum <- which(rownames(nb$nb)==x) |>
            as.integer()
        }
      }
      if(is.character(y)){
        if(is.list(nb$nb)){
          ynum <- which(names(nb$nb)==y) |>
            as.integer()
        }
        if(is.matrix(nb$nb)){
          ynum <- which(rownames(nb$nb)==y) |>
            as.integer()
        }
      }
    }else{
      if(is.list(nb)){
        xnum <- which(names(nb)==x) |>
          as.integer()
        ynum <- which(names(nb)==y) |>
          as.integer()
      }
      if(is.matrix(nb)){
        xnum <- which(rownames(nb)==x) |>
          as.integer()
        ynum <- which(rownames(nb)==y) |>
          as.integer()
      }
    }

  }

  # for cases when dataframe provided...
  if(is.data.frame(nb)){
    tempnb <- nb$nb # list or matrix within dataframe

    if(is.matrix(tempnb)){ # matrix within dataframe
      tempnb <- spdep::mat2listw(tempnb, style="B")
      tempnb <- tempnb[2]
      tempnb <- tempnb$neighbours
    }
    # class(tempnb) <- c("nb","list")
  }else{

    # now tempnb is a nb/list

    # when just a matrix provided, tempnb becomes a list
    if(is.matrix(nb)){
      tempnb <- spdep::mat2listw(nb, style="B")
      tempnb <- tempnb[2]
      tempnb <- tempnb$neighbours
      # class(tempnb) <- c("nb","list")
    }

    # when just a list provided, it is renamed tempnb
    if(is.list(nb)){
      tempnb <- nb
      # class(tempnb) <- c("nb","list")
    }
  }
  # now that tempnb has been created for different circumstances...

  # if x and y are already neighbours, return original structure unchanged
  if(!xnum %in% tempnb[[ynum]])
  {
    return(nb)
  }else # else perform alteration, creating a list called tempnb
  {
    tempnb2 <- tempnb
    tempnb2[[xnum]] <- tempnb[[xnum]][tempnb[[xnum]] !=ynum] |>
      sort()
    tempnb2[[ynum]] <- tempnb[[ynum]][tempnb[[ynum]] !=xnum] |>
      sort()

    class(tempnb2) <- c("nb","list")

    if(is.data.frame(nb)){
      if(is.matrix(nb$nb)){
        tempmat <- spdep::nb2mat(tempnb2, style = "B")
        dfmat_return <- nb
        dfmat_return$nb <- tempmat
        return(dfmat_return)
      }
      if(is.list(nb$nb)){
        dflist_return <- nb
        dflist_return$nb <- tempnb2
        return(dflist_return)
      }
    }else{
      # return in appropriate form
      if(is.list(nb)){
        return(tempnb2)
      }
      if(is.matrix(nb)){
        tempmat <- spdep::nb2mat(tempnb2, style = "B")
        return(tempmat)
      }
    }
  }
}
