
# RELSA <img src="https://talbotsr.com/RELSA/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mytalbot/RELSA.svg?branch=master)](https://travis-ci.org/r-lib/usethis)
<!-- badges: end -->

## Relative Severity Score (RELSA)

The RELSA package contains a set of functions for **assessing relative
severity in laboratory animals**. In animal-based research, the problem
of severity classification is crucial. As animals cannot communicate
their state of well-being, scientists need reliable tools for monitoring
severity as closely as possible. It has been shown that a diversity of
behavioral tests (and others) may serve this purpose. However, the main
issue with these approaches is that they are rather specific and
challenging to transfer. A comprehensive and easy-to-use toolbox for
assessing and comparing different variables and animal models is
missing. RELSA offers the first glimpse into this matter by combining
any set of experimental outcome variables into a single composite score.
\#\# Installation

You can install the development version of RELSA by running:

    devtools::install_github("mytalbot/relsa")
    library(RELSA)

## Documentation

This package is documented using pkgdown, and the resulting website is
available [here](https://talbotsr.com/RELSA), where detailed tutorials
can be found covering aspects of package functionality. See reference
section for detailed function documentation.

## Example

``` r
library(RELSA)

# Build model -------------------------------------------------------------
raw          <- surgery
vars         <- c("bwc", "burON", "hr", "hrv", "temp", "act")
turnvars     <- c("hr", "temp" )
pre          <- relsa_norm(cbind(raw[,1:4], raw[,vars]), 
                           normthese = c("burON", "hr", "hrv", "temp", "act"), ontime = 1)
bsl          <- relsa_baselines(dataset = pre, bslday = -1, variables = vars, turnvars = turnvars)
levels       <- relsa_levels(pre, bsl = bsl, drops = NULL, turns = c("hr", "temp"),
                             k = 4, customCol = c("red", "green", "blue", "magenta"))

# Test model --------------------------------------------------------------
animal       <- 1
RELSA        <- relsa(set = pre, bsl, a = animal, 
                      drop= NULL, turnvars = turnvars)
head(RELSA$relsa$rms)
```

    ##    rms
    ## 1 0.00
    ## 2 0.73
    ## 3 0.55
    ## 4 0.44
    ## 5 0.44
    ## 6 0.41
