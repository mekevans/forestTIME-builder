# The `add-annual-carbon-good` branch

This branch is a WIP containing Renata's work through October 2024 on (1) interpolating measurements of HT/DIA across remeasurement periods using different algorithms and (2) using David Walker's code to estimate carbon variables on these interpolated values. These objectives were identified as additional aims for forestTIME in September 2024. 

To use this version of the database, see the ["carbon" document here](https://github.com/diazrenata/forestTIME-builder/blob/add-annual-carbon-good/carbon.md). 

## Status of this branch

- This version implements two interpolation methods that are different from the one used in the `pre_carbon` branch. On the `pre_carbon` branch, trees that die are assumed to have died in the survey year in which they are first recorded as dead. This branch uses a "midpoint" and a "mortyr" method. In the "midpoint" method, dead trees are assumed to have died at the midpoint of the remeasurement year in which they died (e.g. a tree recorded as alive in 2010 and dead in 2020 is assumed to have died in 2015). The "mortyr" method is the same as the midpoint, **except** that if a tree has a `MORTYR` recorded, the tree is assumed to have died in `MORTYR`. 
- There have been additional interpolation methods discussed, e.g. taking into account disturbance years and types. This branch does not do any of that.
- Interpolation continues for as long as a tree remains in the database. It stops in the last survey year in which the tree is recorded. _TODO that has been discussed, but not implemented: Extend the interpolation period to the midpoint of the remeasurement period immediately after the last record of a tree._
- Annualized measurements are generated only for trees that meet these criteria: Either alive in all survey periods, or alive in some surveys and dead in others; **and** have no NA measurements for HT or DIA. This excludes a lot (30-40%) of trees. Many trees can be re-added by adding trees that are always recorded as dead and/or have some NA measurements. NA measurements are likely to break the current code, so one solution is to filter the *measurements* to remove individual surveys with NA measurements but keep other records for that tree. I (Renata) began investigating this in the `allow_more` branch, but it may be equally efficient to start this work fresh. 
- Some trees have NA measurements sandwiched between non-NA measurements. We could get additional information by interpolating the NA measurements from the adjoining non-NA measurements. This branch does not attempt any of this.
- This branch does not filter at all based on DIA. Trees with DIA < 5 are saplings measured on the subplot. Carbon estimated from these trees should be (somehow) scaled up to proportionally account for the area of the full plot. Or, these trees could be filtered out until they reach DIA >= 5. This branch does neither of these things.
- This branch generates NSVB carbon variable estimates based on interpolated annual measurements of HT and DIA using the two interpolation methods. It does so using David Walker's code copied essentially verbatim from the `nsvb_test.zip` file he sent the group in September 2024. 
- The carbon estimation procedure runs locally. It runs for some states in GH actions but fails for others with a timeout. I (Renata) believe this is because it takes a lot of time/memory to run the carbon estimation on states with a lot of trees (e.g. Minnesota, Wisconsin). Currently, I just don't run this on those states in GH actions. To get it running, you'd need to either speed up the carbon estimation code or break the states into smaller units (e.g. counties).
- Edge effects - the carbon estimates appear to decline after around 2010. This is not a real effect but reflects that fewer and fewer trees have been resampled since their last survey visit. It should be corrected for, either by scaling estimates to the number of plots resampled or by extrapolating measurements for upcoming surveys.


# forestTIME-builder
Scripts to generate a forestTIME database. Ancestral code is in forestTIME and automatic-trees, which has a bloated git history. 

The code in this repo will download raw FIA data from DataMart, process it to create forestTIME tables, and store these tables as (currently) .parquet files. This section of the workflow can be run in parallel broken out by state. These tables are then stacked to create one database with forestTIME tables for the whole country. This database can then be uploaded and shared, e.g. via Zenodo or Box. 

I anticipate that *most* users of forestTIME will not run this code. Instead it will run automatically via github actions and push the finished database someplace accessible to users, who will then download it and query it. However, anyone who wants to can download this repo and run the scripts to create a database locally. 

## Organization

- `R` contains functions to download data, create tables, and add them to the database. These functions hardly ever change.
- `scripts` contains a workflow to run the functions in `R` to generate a database and push it to Zenodo. These workflows have undergone a lot of recent change to navigate trade-offs in terms of local vs. automated, all at once vs. state by state, etc. To generate a forestTIME .duckdb, run the scripts in `scripts` in order/following the instructions in the comments.

## Automation and Zenodo push

- These scripts run automatically via GitHub actions, currently on a push to this branch. This can be updated to a scheduled job.
- One workflow runs for each state, generating state-level database tables which are stored as .parquet files. The .parquet files are stored as GitHub artifacts. A final workflow runs to stack all of the state-level tables into one database, which is uploaded to a Zenodo archive. This is currently private, located at: https://zenodo.org/records/13377070. This can be updated to a public archive when we are ready.
- To modify the workflow scripts, *don't* modify the files in `scripts/01-state-by-state`. Instead modify the text in `scripts/create-workflow-yml.R` and then run that script to automatically generate the state-level scripts.
- To set up a push to Zenodo from GitHub actions, generate a Zenodo token in your Zenodo account and supply this as an environment variable as an Actions secret in the GitHub repository. 


------------------------------------------------------------------------
Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) team
