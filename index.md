
RELSA <img src="https://talbotsr.com/RELSA/logo.png" align="right" width="120" />
=================================================================================

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/mytalbot/RELSA.svg?branch=master)](https://travis-ci.org/r-lib/usethis) <!-- badges: end -->

Relative Severity Score (RELSA)
-------------------------------

The RELSA package contains a set of functions for **assessing relative severity in laboratory animals**. In animal-based research the problem of severity classification is crucial. As animals cannot communicate their state of well-being, scientists need reliable tools for monitoring severity as closely as possible. It has been shown that a diversity of behavioural tests (and others) may serve this purpose. However, the main issue with these approaches is that they are rather specific and difficult to transfer. A comprehensive and easy to use toolbox for the assessment and comparison of different variables and animal models is missing. RELSA offers a first glimpse into this matter by combining any set of experimental outcome variables into a single composite score.

Installation
------------

You can install the development version of RELSA by running:

    devtools::install_github("mytalbot/relsa")
    library(RELSA)

Documentation
-------------

This package is documented using pkgdown, and the resulting website is available [here](https://talbotsr.com/RELSA), where detailed Tutorials can be found covering all of the package functionality. See reference for detailed function documentation.

Example
-------

``` r
library(RELSA)

# Build model -------------------------------------------------------------
raw          <- postop
vars         <- c("bwc", "bur2h", "burON", "hr", "hrv", "temp", "act", "mgs")
turnvars     <- c("hr", "mgs", "temp" )
pre          <- relsa_norm(cbind(raw[,1:4], raw[,vars]), 
                           normthese = c("bur2h", "burON", "hr", "hrv", "temp", "act", "mgs"), ontime = 1)
bsl          <- relsa_baselines(dataset = pre, bslday = -1, variables = vars, turnvars = turnvars)
levels       <- relsa_levels(pre, bsl = bsl, drops = c("bw", "score"), turns = c("hr", "mgs", "temp"),
                             k = 4, customCol = c("red", "green", "blue", "magenta"))

# Test model --------------------------------------------------------------
animal       <- 1
RELSA        <- relsa(set = pre, bsl, a = animal, 
                      drop= c("bw", "score", "mgs30", "mgs180"), turnvars = turnvars)
head(RELSA$relsa$rms)
```

    ##    rms
    ## 1 0.00
    ## 2 0.68
    ## 3 0.49
    ## 4 0.40
    ## 5 0.44
    ## 6 0.35
