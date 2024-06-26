% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/st_manual_cut_nb.R
\name{st_manual_cut_nb}
\alias{st_manual_cut_nb}
\title{Manual remove contiguity between two areas}
\usage{
st_manual_cut_nb(nb, x, y)
}
\arguments{
\item{nb}{a neighbourhood `"list"` or `"matrix"`, or an `sf` dataframe with a neighbourhood column called `"nb"`.}

\item{x}{name or number of first area.}

\item{y}{name or number of second area.}
}
\value{
An amended neighbourhood `"list"`, `"matrix"`, or `sf` dataframe with a neighbourhood column called `"nb"`.
}
\description{
Manual remove contiguity between two areas
}
\examples{
st_bridges(uk_election,"constituency_name") |>
st_manual_cut_nb("Ynys Mon","Arfon") |>
st_manual_cut_nb(292,378)
}
