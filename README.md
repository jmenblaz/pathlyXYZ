
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pathlyXYZ <a href='https://github.com/jmenblaz/pathlyXYZ'><img src="man/figures/pathlyXYZ_logo_bg_white.svg" align="right" height="200"/></a>

<!-- badges: start -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)

[![Status:
development](https://img.shields.io/badge/status-development-blue.svg)](#)

[![License:
MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

[![Last
commit](https://img.shields.io/github/last-commit/jmenblaz/pathlyXYZ)](https://github.com/jmenblaz/pathlyXYZ/commits/main)

<!-- badges: end -->
<!-- badges: end -->

`pathlyXYZ` is an open-access R package for simulating animal movement
paths in three dimensions (X, Y, Z) from empirical bio-logging data.
Unlike most existing tools that operate in two-dimensional space,
**pathlyXYZ** enables the generation of fully 3D trajectories,
explicitly accounting for vertical movement alongside horizontal
displacement.

The package is being designed (experimental stage) to be flexible and
extensible, allowing future integration of alternative movement
representations such as 2.5D simulations within a unified framework.
This makes **pathlyXYZ** particularly suitable for studying movement
processes in marine, aerial, and terrestrial systems where vertical
space plays a key ecological role.

Potential `pathlyXYZ` applications include, but are not limited to, the
generation of pseudo-absence or background trajectories for Species
Distribution Models (SDMs), methodological benchmarking, and
simulation-based movement ecology studies.

**Keywords:** *movement-ecology · bio-logging · 3D Movement ·
animal-tracking · trajectory simulation · vertical dimension*

------------------------------------------------------------------------

## Installation

The development version of pathlyXYZ will be available from
[GitHub](https://github.com/) and realesed from CRAN.

## Example

The \`pathlyXYZ\`\` **package is currently under active development**,
and it has not yet a stable published version. Reproducible, end-to-end
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

Users interested in the current development state are encouraged to
explore the source code and follow the project’s progress on GitHub.

## Contributing and Code of Conduct

The \`pathlyXYZ\`\` package will be released with a Contributor Code of
Conduct. By contributing to this project, you agree to abide by its
terms.

Well-intentioned contributions are very welcome. Suggestions, bug
reports, improvements, or new features can be proposed by opening an
issue or submitting a pull request via the project’s GitHub repository.

## How to cite pathlyXYZ

The **pathlyXYZ** R package is currently under development. A formal
manuscript is in preparation. In the meantime, the package can be cited
as:

> Menéndez-Bláquez, J. (in prep). **pathlyXYZ: An R package for
> simulating and analyzing multidimensional movement trajectories**.
> Preprint available on [arXiv](arXiv:XXXX.XXXXX). DOI: to be assigned.
