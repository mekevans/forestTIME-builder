source(here::here("R", "import_tables_from_csvs.R"))
source(here::here("R", "add_cns_to_db.R"))
source(here::here("R", "add_qa_flags_to_db.R"))
source(here::here("R", "add_info_table_to_db.R"))
source(here::here("R", "add_tree_annualized_to_db.R"))
source(here::here("R", "add_sapling_transitions_to_db.R"))


#' Create all tables for foresttime db
#'
#' @param con db connection
#' @param rawdat_dir where the raw csvs are
#' @param delete_downloads whether or not to delete downloaded .csv files after adding them to db.
#'
#' @return nothing
#' @export
#'
create_all_tables <- function(con, rawdat_dir, delete_downloads = F) {
  import_tables_from_csvs(con = con,
                          csv_dir = rawdat_dir)
  if(delete_downloads) {
    list.files(rawdat_dir,
               recursive = T,
               full.names = T) |> 
      file.remove()
  }
  
  add_cns_to_db(con)
  add_qa_flags_to_db(con)
  add_info_table_to_db(con)
  add_annual_estimates_to_db(con)
  add_saplings_to_db(con)
  
}