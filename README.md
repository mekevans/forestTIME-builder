# forestTIME-builder

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![.github/workflows/create_db.yml](https://github.com/mekevans/forestTIME-builder/actions/workflows/create_db.yml/badge.svg)](https://github.com/mekevans/forestTIME-builder/actions/workflows/create_db.yml)
[![R-CMD-check](https://github.com/mekevans/forestTIME-builder/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mekevans/forestTIME-builder/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This R package contains functions to create annualized versions of FIA data. It can download raw FIA data from DataMart, merge required tables, interpolate between surveys, and re-estimate biomass and carbon from interpolated values.  The output is a single dataframe with values for every tree in every year rather than the original panel design.

## Installation

`forestTIME.builder` is not on CRAN.

To install from r-universe:

```r
install.packages("forestTIME.builder", repos = c("https://cct-datascience.r-universe.dev", getOption("repos")))
```

To install from GitHub:

```r
#install.packages("pak")
pak::pak("mekevans/forestTIME-builder")
```

## Contributing

If you'd like to make modifications to the functions in this package, you are welcome to do so.  However, it would be ideal if those modifications could be incorporated into this package to benefit everyone using it and to keep a single "source of truth" for this workflow. 

If you are brand new to R package development or using GitHub, I recommend you start by opening an issue to make a suggestion or report a bug.  If you have some familiarity with R code and feel comfortable, feel free to make a pull request. I recommend using `usethis` to handle this process if you are not familiar with git and GitHub.  Start by [setting up your GitHub credentials](https://usethis.r-lib.org/articles/git-credentials.html) and then use `usethis::create_from_github("mekevans/forestTIME-builder")` to (possibly fork) and clone this repository. The `usethis` package has some [nice documentation](https://usethis.r-lib.org/articles/pr-functions.html) on how to create pull requests using it's `pr_*()` functions, namely `pr_init()` to create a new branch, `pr_push()` to actually open the pull request on GitHub, and `pr_finish()` to clean things up after yoru pull request is merged.

## Automation

Eventually, this repository will contain a GitHub workflow to automate the interpolation and carbon estimation for all states and provide the resulting annualized dataset(s).

## Citation

To cite this work, please use:

> Scott E.R., Diaz R., Steinberg D., Riemer K., Evans M. (2025).
> “forestTIME.builder: Generate Annualized Carbon and Biomass Estimates From FIA
  Data” <https://github.com/mekevans/forestTIME-builder>.

Please also cite Westfall et al. (2024):

> Westfall, J.A., Coulston, J.W., Gray, A.N., Shaw, J.D., Radtke, P.J., Walker, D.M., Weiskittel, A.R., MacFarlane, D.W., Affleck, D.L.R., Zhao, D., Temesgen, H., Poudel, K.P., Frank, J.M., Prisley, S.P., Wang, Y., Sánchez Meador, A.J., Auty, D., Domke, G.M., 2024.
> A national-scale tree volume, biomass, and carbon modeling system for the United States.
> U.S. Department of Agriculture, Forest Service.
> <https://doi.org/10.2737/wo-gtr-104>

------------------------------------------------------------------------

Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) team
