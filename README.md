
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pathlyXYZ <a href='https://github.com/jmenblaz/pathlyXYZ'><img src="man/figures/pathlyXYZ_logo_bg_white.svg" align="right" height="200"/></a>

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/1154606375.svg)](https://doi.org/10.5281/zenodo.18770515)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Status:
development](https://img.shields.io/badge/status-development-blue.svg)](#)
[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Last
commit](https://img.shields.io/github/last-commit/jmenblaz/pathlyXYZ)](https://github.com/jmenblaz/pathlyXYZ/commits/main)

<!-- badges: end -->

**`pathlyXYZ`** is an open-access R package for simulating, exploring
and visualizing animal movement paths in three dimensions (X, Y, Z) from
empirical bio-logging data. Unlike most existing tools that operate in
two-dimensional space, `pathlyXYZ` enables the generation of fully 3D
trajectories, explicitly accounting for vertical movement alongside
horizontal displacement.

The package is being designed (experimental stage) to be flexible and
extensible, allowing future integration of alternative movement
representations such as 2.5D simulations within a unified framework.

While `pathlyXYZ` is particularly motivated by and suited for marine
enviroments, where the vertical dimension plays a fundamental ecological
and physical role, its conceptual and computational framework is
designed to be applicable to aerial and terrestrial movement processes
as well, wherever three-dimensional space is ecologically relevant.

Potential applications of `pathlyXYZ` extend to the generation of
pseudo-absence or background trajectories for Species Distribution
Models (SDMs), simulation-based inference and hypothesis testing,
benchmarking of analytical workflows, and the quantitative exploration
of movement processes in fully three-dimensional space.

**Keywords:** *movement-ecology · bio-logging · 3D Movement ·
animal-tracking · trajectory simulation · vertical dimension*

------------------------------------------------------------------------

## Development Status

> ⚠️ **Development status:** `pathlyXYZ` is currently in an early
> experimental stage. Core simulation functions are under active
> development and results should not yet be used for scientific
> inference.

## Installation

The package is currently under active development and is not yet
installable.

Installation instructions will be provided once the first functional
release is available on GitHub. A stable version is planned for future
submission to CRAN.

## Example

`pathlyXYZ` **package is currently under active development**, and it
has not yet a stable published version. Reproducible, end-to-end
examples will be added once the core functions are finalized.

At this stage, the package focuses on defining the conceptual and
methodological framework for simulating three-dimensional movement
trajectories from empirical bio-logging data, as well as create tools
for visualize and explore this data. Future versions of this README will
include worked examples illustrating typical use cases, such as:

- Simulation of fully 3D movement paths from observed tracking data
  (marine, aerial and terrestrial environment) by different modelling
  methods
- Visualize and ploting shor-cuts and tools in order to facility the use
  of vertical dimension in the use of bio-logging data

## Code of Conduct

The `pathlyXYZ` package will be released with a Contributor Code of
Conduct. By contributing to this project, you agree to abide by its
terms.

As `pathlyXYZ` is currently in an early experimental stage, we
particularly welcome well-intentioned conceptual suggestions,
methodological ideas, and feature proposals that may help shape the
future development of the package.

If you would like to propose an idea, suggest an improvement, or report
a potential issue, please open an *Issue* in the repository. For
substantial code contributions, we recommend starting with a discussion
via an *Issue* before submitting a pull request due the current dev.
stage.

## How to cite pathlyXYZ

The **`pathlyXYZ`** R package is currently under active development and
a formal methodological manuscript is in preparation.

If you wish to cite the theoretical and conceptual framework of
pathlyXYZ at its current development stage, please cite the GitHub
repository:

> Menéndez-Bláquez, J. (Year). *pathlyXYZ: An R package for simulating
> and analyzing multidimensional movement trajectories* (development
> version). GitHub - <a href="https://github.com/jmenblaz/pathlyXYZ"
> class="uri">github.com/jmenblaz/pathlyXYZ</a>; DOI:

Once a stable release and/or peer-reviewed publication becomes
available, users are encouraged to cite the corresponding
version-specific DOI and associated manuscript.
