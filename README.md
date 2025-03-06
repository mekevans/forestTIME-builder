# forestTIME-builder

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip) [![.github/workflows/create_db.yml](https://github.com/mekevans/forestTIME-builder/actions/workflows/create_db.yml/badge.svg)](https://github.com/mekevans/forestTIME-builder/actions/workflows/create_db.yml)

<!-- badges: end -->

Scripts to generate a forestTIME database.
Ancestral code is in forestTIME and automatic-trees, which has a bloated git history.

The code in this repo will download raw FIA data from DataMart, process it to create forestTIME tables, and store these tables as (currently) parquet files.
This section of the workflow can be run in parallel broken out by state.
These tables are then stacked to create one database with forestTIME tables for the whole country.
This database can then be uploaded and shared, e.g. via Zenodo or Box.

I anticipate that *most* users of forestTIME will not run this code.
Instead it will run automatically via GitHub Actions and push the finished database someplace accessible to users, who will then download it and query it.
Functions to query an already-generated database can be found in <https://github.com/mekevans/forestTIME/>.
However, anyone who wants to can download this repo and run the scripts to create a database locally.

This is a work in progress.
Find additional notes and documentation in .md files in the `docs/` folder.

## The `pre_carbon` branch

The `pre_carbon` contains the stable version of forestTIME prior to the addition of NSVB carbon estimation.

## Organization

-   `R/` contains functions to download data, create tables, and add them to the database. These functions hardly ever change.
-   `scripts/` contains a workflow to run the functions in `R` to generate a database and push it to Zenodo. These workflows have undergone a lot of recent change to navigate trade-offs in terms of local vs. automated, all at once vs. state by state, etc. To generate a forestTIME .duckdb, run the scripts in `scripts` in order/following the instructions in the comments.
-   `carbon_code/` contains code and data from David Walker for estimating carbon
-   `docs/` contains Quarto documents explaining and exploring various aspects of this codebase.
-   `renv/` is set up by the `renv` package (see [Reproducibility](#reproducibility))

## Reproducibility {#reproducibility}

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) to manage R package dependencies.
Run `renv::restore()` to install all the required packages with the correct versions to run the code.
If you install or update a package, run `renv::snapshot()` to update the `renv.lock` file.

To generate the database, run the code in `scripts/` in order starting with `01-run_locally.R`.
Currently the `03-upload_parquet_db_zenodo.R` script will not work.

## Automation

The GitHub action to generate the database is currently disabled until we figure out how to shrink the database size significantly.

## Citation

To cite this work, please use:

> Diaz R, Scott E, Steinberg D, Riemer K, Evans M (2025).
> “forestTIME-builder: generating annualized carbon and biomass estimates from FIA data.” <https://github.com/mekevans/forestTIME-builder>.

Please also cite Westfall et al. (2024):

> Westfall, J.A., Coulston, J.W., Gray, A.N., Shaw, J.D., Radtke, P.J., Walker, D.M., Weiskittel, A.R., MacFarlane, D.W., Affleck, D.L.R., Zhao, D., Temesgen, H., Poudel, K.P., Frank, J.M., Prisley, S.P., Wang, Y., Sánchez Meador, A.J., Auty, D., Domke, G.M., 2024.
> A national-scale tree volume, biomass, and carbon modeling system for the United States.
> U.S. Department of Agriculture, Forest Service.
> <https://doi.org/10.2737/wo-gtr-104>

<!--
## Automation and Zenodo push

-   These scripts run automatically via GitHub actions, currently on a push to this branch. This can be updated to a scheduled job.
-   One workflow runs for each state, generating state-level database tables which are stored as .parquet files. The .parquet files are stored as GitHub artifacts. A final workflow runs to stack all of the state-level tables into one database, which is uploaded to a Zenodo archive. This is currently private, located at: <https://zenodo.org/records/13377070>. This can be updated to a public archive when we are ready.
-   To set up a push to Zenodo from GitHub actions, generate a Zenodo token in your Zenodo account and supply this as an environment variable as an Actions secret in the GitHub repository.
-->

------------------------------------------------------------------------

Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) team
