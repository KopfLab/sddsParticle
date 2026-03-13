
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sddsParticle

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of sddsParticle is to provide a web GUI to interact with
[SDDS](https://github.com/mLamneck/SDDS) microcontrollers running with
[particleSpike](https://github.com/KopfLab/SDDS_particleSpike) on
particle devices.

## Installation

You can install the development version of sddsParticle from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("KopfLab/sddsParticle")
```

## Example

``` r
library(sddsParticle)

# run once to safely store your particle token in your OS keychain
particle_store_token()

# now run the GUI
sdds_run_gui()
```
