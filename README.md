# forestTIME-builder
Scripts to generate a forestTIME database. Ancestral code is in forestTIME and automatic-trees, which has a bloated git history. 

The code in this repo will download raw FIA data from DataMart, process it to create forestTIME tables, and store these tables as (currently) .parquet files. This section of the workflow can be run in parallel broken out by state. These tables are then stacked to create one database with forestTIME tables for the whole country. This database can then be uploaded and shared, e.g. via Zenodo or Box. 

I anticipate that *most* users of forestTIME will not run this code. Instead it will run automatically via github actions and push the finished database someplace accessible to users, who will then download it and query it. However, anyone who wants to can download this repo and run the scripts to create a database locally. 

## Organization

- `R` contains functions to download data, create tables, and add them to the database. These functions hardly ever change.
- `scripts` contains a workflow to run the functions in `R` to generate a database and push it to Zenodo. These workflows have undergone a lot of recent change to navigate trade-offs in terms of local vs. automated, all at once vs. state by state, etc. To generate a forestTIME .duckdb, run the scripts in `scripts` in order/following the instructions in the comments.

## TODOs

Current to-do items are:

- (Re)-modularizing the workflow in `scripts` so that it is easy to generate a forestTIME database from *already downloaded* data files. This is fully supported by the underlying functions and is just not how this particular iteration of the workflow is set up.
- Troubleshooting the automated workflow to run without burning up too much GH actions bandwidth or creating an impossibly large git repo. Ideas: (1) use .csvs instead of parquets; (2) push everything to Zenodo archives instead of git. 


------------------------------------------------------------------------
Developed in collaboration with the University of Arizona [CCT Data Science](https://datascience.cct.arizona.edu/) team