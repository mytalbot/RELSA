
# RELSA <img src="man/figures/logo.png" align="right" width="120" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mytalbot/RELSA.svg?branch=master)](https://travis-ci.org/r-lib/usethis)
<!-- badges: end -->

## Relative Severity Assessment Score (RELSA)

The RELSA package contains a set of functions for **assessing relative
severity in laboratory animals**. In animal-based research the problem
of severity classification is crucial. As animals cannot communicate
their state of well-being, scientists need reliable tools for monitoring
severity as closely as possible. It has been shown that a diversity of
behavioural tests (and others) may serve this purpose. However, the main
issue with these approaches is that they are rather specific and
difficult to transfer. A comprehensive and easy to use toolbox for the
assessment and comparison of different variables and animal models is
missing. RELSA offers a first glimpse into this matter by combining any
set of experimental outcome variables into a single composite score.

## Installation

You can install the development version of RELSA by running:

    devtools::install_github("mytalbot/relsa", build_vignettes = TRUE)
    library(RELSA)

## Documentation

This package is documented using pkgdown, and the resulting website is
available [here](https://talbotsr.com/RELSA), where detailed tutorials
can be found covering aspects of package functionality. See reference
section for detailed function documentation.

## Data availability

Raw data with labelled subgroups (treatment/condition columns) can be
found in the GitHub repository folder “raw\_data”. Data are stored in
four \*.txt files. [Here is the direct
link](https://github.com/mytalbot/RELSA/tree/master/raw_data). The data
in the file “tm\_post-op.txt” is also internalized in the RELSA package
and can be called with: `RELSA::postop`.

## Example

``` r
library(RELSA)

# Build model -------------------------------------------------------------
raw          <- RELSA::postop
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
#>    rms
#> 1 0.00
#> 2 0.69
#> 3 0.49
#> 4 0.40
#> 5 0.44
#> 6 0.35
```
