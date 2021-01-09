# tidygedcom.internals

<!-- badges: start -->

[![R-CMD-check](https://github.com/jl5000/tidygedcom.internals/workflows/R-CMD-check/badge.svg)](https://github.com/jl5000/tidygedcom.internals/actions)
[![](https://codecov.io/gh/jl5000/tidygedcom.internals/branch/master/graph/badge.svg)](https://codecov.io/gh/jl5000/tidygedcom.internals)
[![CodeFactor](https://www.codefactor.io/repository/github/jl5000/tidygedcom.internals/badge)](https://www.codefactor.io/repository/github/jl5000/tidygedcom.internals)
[![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

This package contains the internal data structures used in the [`tidygedcom`](https://github.com/jl5000/tidygedcom) package. It codifies the rules in the [GEDCOM 5.5.5 specification](https://www.gedcom.org/gedcom.html).

It is only to be used by developers of tidygedcom extensions.

## Installation

You can install the development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("jl5000/tidygedcom.internals")
```

