# forestTIME-builder

Scripts to generate a forestTIME database.
Ancestral code is in forestTIME and automatic-trees, which has a bloated git history.

The code in this repo will download raw FIA data from DataMart, process it to create forestTIME tables, and store these tables as (currently) .parquet files.
This section of the workflow can be run in parallel broken out by state.
These tables are then stacked to create one database with forestTIME tables for the whole country.
This database can then be uploaded and shared, e.g. via Zenodo or Box.

I anticipate that *most* users of forestTIME will not run this code.
Instead it will run automatically via github actions and push the finished database someplace accessible to users, who will then download it and query it.
Functions to query an already-generated database can be found in <https://github.com/mekevans/forestTIME/>.
However, anyone who wants to can download this repo and run the scripts to create a database locally.

## The `pre_carbon` branch

The `pre_carbon` contains the stable version of forestTIME prior to the addition of NSVB carbon estimation. 

## Organization

-   `R` contains functions to download data, create tables, and add them to the database. These functions hardly ever change.
-   `scripts` contains a workflow to run the functions in `R` to generate a database and push it to Zenodo. These workflows have undergone a lot of recent change to navigate trade-offs in terms of local vs. automated, all at once vs. state by state, etc. To generate a forestTIME .duckdb, run the scripts in `scripts` in order/following the instructions in the comments.

## Automation and Zenodo push

-   These scripts run automatically via GitHub actions, currently on a push to this branch. This can be updated to a scheduled job.
-   One workflow runs for each state, generating state-level database tables which are stored as .parquet files. The .parquet files are stored as GitHub artifacts. A final workflow runs to stack all of the state-level tables into one database, which is uploaded to a Zenodo archive. This is currently private, located at: <https://zenodo.org/records/13377070>. This can be updated to a public archive when we are ready.
-   To modify the workflow scripts, *don't* modify the files in `scripts/01-state-by-state`. Instead modify the text in `scripts/create-workflow-yml.R` and then run that script to automatically generate the state-level scripts.
-   To set up a push to Zenodo from GitHub actions, generate a Zenodo token in your Zenodo account and supply this as an environment variable as an Actions secret in the GitHub repository.

------------------------------------------------------------------------

Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) team
