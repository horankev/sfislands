
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/logo.png" align="right" height="240"/>

# sfislands

<!-- badges: start -->
<!-- badges: end -->

The goal of `sfislands` is to make it easier to deal with geographic
datasets which contain islands. It does so using a tidy framework in the
spirit of Josiah Parry’s [sfdep](https://sfdep.josiahparry.com/)
package.

- These do not have to be “literal” islands but any situation where
  discontiguous geographical units are present.

Such a situation can lead to two issues.

- Firstly, if unaddressed, the presence of such islands or exclaves can
  make certain types of contiguity-based modelling impossible.

- Secondly, just because two areas are separated by, say, a body of
  water, this does not necessarily mean that they are to be considered
  independent of each other.

This package offers solutions to allow for the inclusion or exclusion of
these units within an uncomplicated workflow.

## Installation

You can install the development version of `sfislands` from
[GitHub](https://github.com/horankev/sfislands) with:

``` r
# install.packages("devtools")
devtools::install_github("horankev/sfislands")
```

## Summary of features

1.  The initial setting up neighbourhood structures can be frustrating
    for people who are eager to get started with fitting spatial models.
    This is especially so when the presence of discontiguities within a
    geographical dataset means that, even having set up a neighbours
    list, the model will still not run without further awkward data
    manipulations.

2.  As an aid to setting up neighbourhood structures, particularly when
    islands are involved, the package has a function to quickly map any
    neighbourhood structure for visual inspection. This can also be used
    to examine the output of `sfdep` neighbour functions. Such maps can
    be used to check if the structure makes sense, given the
    researcher’s knowledge about the geography of the study area.

3.  If there are some neighbours assigned which are not appropriate, or
    if you wish to add additional ones, there are functions to allow
    this to be done in a straightforward and openly reportable way.

4.  Once an appropriate neighbourhood structure is in place, different
    types of statistical tests and models can be performed. `sfdep`
    contains functionality to perform such test, and the output from
    `sfislands` can be used in its functions.

5.  The contiguity outputs from `sfislands` can be directly used to fit
    different types of (multilevel) (I)CAR models using, for example,
    the `mgcv`, `brms`, `stan` or `INLA` packages.

6.  For `mgcv` in particular, the predictions of such models can be
    quite tedious to extract and visualise. `sfislands` can streamline
    this workflow from the human side. Furthermore, there is a function
    to draw maps of these predictions for quick inspection.

## Functions overview

The following is a framework within which the `sfislands` functions
could be used:

### Step 1: Set up data (“*pre-functions*”)

| function:               | purpose:                                                                                                                                   |
|-------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| **st_bridges()**        | *Create a neighbours list, matrix, or `sf` dataframe containing a neighbours list or matrix as column “nb”, while accounting for islands.* |
| **st_quickmap_nb()**    | *Check contiguities visually on map.*                                                                                                      |
| **st_check_islands()**  | *Check assignment of island contiguities in a dataframe.*                                                                                  |
| **st_manual_join_nb()** | *Make manual changes to any connections.*                                                                                                  |
| **st_manual_cut_nb()**  | *Make manual changes to any connections.*                                                                                                  |

### Step 2: Create model

*Use the output of **st_bridges()** as both the data and neighbourhood
inputs for a model using, for example, `mgcv`, `brms` or `inla`.*

### Step 3: Examine output (“*post functions*”)

| function:               | purpose:                                                 |
|-------------------------|----------------------------------------------------------|
| **st_augment()**        | *Augment the original dataframe with model predictions.* |
| **st_quickmap_preds()** | *Generate quick maps of these predictions.*              |

More details of how to operate these functions are provided in the
vignette.
