
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyged.internals <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidyged.internals/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidyged.internals/actions)
[![](https://codecov.io/gh/jl5000/tidyged.internals/branch/main/graph/badge.svg)](https://codecov.io/gh/jl5000/tidyged.internals)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidyged.internals/badge)](https://www.codefactor.io/repository/github/jl5000/tidyged.internals)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

This package contains the internal data structures used to represent
family tree GEDCOM files. It codifies the rules in the [GEDCOM 5.5.5
specification](https://www.gedcom.org/gedcom.html).

It is only to be used by developers of tidyged and its extensions.

The package is part of the `gedcompendium` ecosystem of packages. This
ecosystem enables the handling of `tidyged` objects (tibble
representations of GEDCOM files), and the main package of this ecosystem
is [`tidyged`](https://jl5000.github.io/tidyged/).

<img src="man/figures/allhex.png" width="65%" style="display: block; margin: auto;" />

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("jl5000/tidyged.internals")
```
