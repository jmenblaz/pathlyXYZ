
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pathlyXYZ <a href='https://ianjonsen.github.io/aniMotum/index.html'><img src='man/figures/pathlyXYZ_logo_bg_white.svg' align="right" height="200" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

`pathlyXYZ` is R package to simulate movement paths in three axis of the
landdcape (X, Y, Z) for empirical bio-logging trajectories. Its main and
novel feature is the ability to simulate fully 3D paths, while
maintaining flexibility to implement, unify, or extend other types of
path simulations such as 2-2.5D in future versions.

Animal movement occurs naturally in three dimensions, not just in a 2D
plane (whether in marine, terrestrial, or aerial environments).
**pathlyXYZ** is an open-access R package for simulating movement paths
in three dimensions (X, Y, Z) from empirical bio-logging trajectories.
Its main novelty is the ability to generate fully 3D trajectories while
maintaining flexibility to implement, unify, or extend other path
simulation approaches, including 2D and 2.5D simulations in coming
versions.

The package has multiple cutting-edge use cases, such as, 1)
**Generating pseudo-absences or background points for Species
Distribution Models (SDMs)**

**Keywords:** Movement-ecologty · Biologging · 3D-Movement ·
Animal-tracking · Trajectory simulation · Vertical dimension\*

------------------------------------------------------------------------

## Installation

You can install the development version of pathlyXYZ from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jmenblaz/pathlyXYZ")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(pathlyXYZ)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
