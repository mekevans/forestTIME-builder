The scripts in this folder execute the same workflow to download raw data from DataMart and create forestTIME tables stored as parquet files for every state. 
Currently the output files are saved and committed to git. This must be changed, either to saving them as .csv files or to saving them to an external archive (box or Zenodo). 


They are copied (and hardcoded) so that they can run in parallel in GitHub Actions. 


You can run all of them on a single machine using the script `scripts/100-run-locally.R`. 
This can be very slow because it requires downloading the raw data for every state. 


A possible to-do item is to break them apart so the download step occurs separate from generating the tables. 
This is fully supported by the underlying functions; I just didn't happen to set up this version of the workflow that way. 