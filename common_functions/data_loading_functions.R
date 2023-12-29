#######################################################################
#
#######################################################################
# Logger setup
data_loading_func_logger <- lgr::get_logger_glue('root/analytics/functions/data_loading')

data_loading_func_logger$debug('Entering data loading functions')

#######################################################################
# Heavily reused and shared functions
#######################################################################
#' Return username associated with a given database name
#' 
#' @param db_name Name of the database
#' 
#' @return A character string containing the username associated with the database
#' 
#' @importFrom keyring key_list
get_db_username <- function(db_name){
  return(keyring::key_list(db_name)[1, 2])
}

#' Compressed File Name Function
#' 
#' Adds a gzip compression ending to the filepath if the file will be compressed
#' 
#' @param file_path The path of the file to consider
#' @param is_compressed Whether the file will be compressed or not
#' 
#' @return The new file name, compressed or otherwise
#' 
#' @examples
#' compressed_file_name("my_file", TRUE)
#' compressed_file_name("my_file", FALSE)
compressed_file_name <- function(file_path, is_compressed){
  new_file_path <- paste(file_path, ifelse(is_compressed, ".gz", ""), sep="")
  return(new_file_path)
}


#' Lowercase Column Names Function
#' 
#' Renames all column names to lower case
#' 
#' @param dataframe_to_reformat A data frame to process
#' 
#' @return A data frame with all column names in lower case
#' 
#' @examples
#' lowercase_columns(mtcars)
lowercase_columns <- function(dataframe_to_reformat){
  dataframe_to_reformat %>% dplyr::rename_all(tolower)
}

#' Upppercase Column Names Function
#' 
#' Renames all column names to upper case
#' 
#' @param dataframe_to_reformat A data frame to process
#' 
#' @return A data frame with all column names in lower case
#' 
#' @examples
#' lowercase_columns(mtcars)
uppercase_columns <- function(dataframe_to_reformat){
  dataframe_to_reformat %>% dplyr::rename_all(toupper)
} 


#' Load and Process Data from SQL Database Function
#' 
#' Loads data from a SQL database and writes a copy to disk as well.
#' 
#' @param sql_connection_engine An ODBC/DBI connection engine that can run and retrieve SQL data
#' @param sql_script The SQL script to be run inside the database
#' @param data_path The path to where the file will be stored on disk
#' @param compress_on_disk A flag indicating whether the file will be additionally compressed
#' @param extra_processing A function doing any extra processing before saving to disk
#' 
#' @return The new data loaded in from the database as a tibble
#' 
#' @examples
#' load_proc_wrt_data(connection_engine, "SELECT * FROM my_table", "my_file", TRUE)
load_proc_wrt_data <- function(sql_connection_engine, sql_script, data_path, compress_on_disk, extra_processing = function(x) x){
  new_data <- tibble::as_tibble(DBI::dbGetQuery(sql_connection_engine, sql_script))
  new_data <- extra_processing(new_data)  # Run an extra current function to make changes
  arrow::write_parquet(new_data, data_path)
  return(new_data)
}


#' Load Data from SQL Database Function
#' 
#' Loads in new data from a SQL database and writes a copy to disk as well.
#' 
#' @param dataset_name The name of the current dataset being processed
#' @param script_paths A list of paths to all relevant SQL scripts
#' @param data_paths A list of paths to where data is/will be stored on disk
#' @param db_engines A list of relevant database connection engines for processing and retrieving SQL
#' @param compress_flag A flag indicating whether to compress file on disk
#' @param ... Any other arguments to feed to other functions
#' 
#' @return The new data loaded in from the database as a tibble
#' 
#' @examples
#' load_from_database("my_dataset", "my_script.sql", "my_data.parquet", db_engine, FALSE)
load_from_database <- function(dataset_name, script_paths, data_paths, db_engines, compress_flag, ...){
  sql_script <- readr::read_file(script_paths[[dataset_name]])
  sql_script <- stringr::str_replace(sql_script, "^\\/\\*[^\n]*\\*\\/\r\n", "")
  return(load_proc_wrt_data(db_engines[[dataset_name]], sql_script, data_paths[[dataset_name]], compress_flag, ...))
}


#' Loads data from disk
#'
#' This function loads in data from disk, either in parquet or delimited format. The function takes in the following parameters:
#'
#' @param dataset_name Name of the current dataset being processed.
#' @param data_paths List of paths to where data is / will be stored on disk.
#' @param compress_flag Flag indicating whether to compress file on disk.
#' @param data_type The type of file that will be retrieved (default: "parquet").
#' @param sep Separator character for delimited files (default: "\t").
#' @param ... Any other arguments to feed to other functions.
#'
#' @return The new data loaded in from the disk as a tibble.
#'
#' @examples
#' load_from_disk("my_dataset", c("f", "data_path2"), TRUE, "delimited", ",", arg1=5, arg2="test")
load_from_disk <- function(dataset_name, data_paths, compress_flag, data_type="parquet", sep="\t", ...){
  
	cur_path <- compressed_file_name(data_paths[[dataset_name]], compress_flag)
    if (data_type == "parquet"){
		out_data <- arrow::read_parquet(cur_path)
	} else if (data_type == "delimited") {
		out_data <- readr::read_delim(cur_path, delim=sep)
	} else if (data_type == "excel"){
	  out_data <- readxl::read_xlsx(cur_path, ...)
	}
	return(out_data)
}

#' Loads SQL data from disk or database
#'
#' This function loads SQL data from either disk or a database, and returns the data as a list. The function takes in the following parameters:
#'
#' @param data_scripts Paths to the SQL script files that create the necessary tables/views.
#' @param data_paths List of paths to where data is / will be stored on disk.
#' @param data_db_engines Database engine(s) to use for loading SQL data.
#' @param compress_files Flag indicating whether to compress file on disk.
#' @param reload_from_db Flag indicating whether to reload the data from the database.
#' @param only_refresh_missing Flag indicating whether to only refresh the missing files.
#' @param data_type The type of file that will be retrieved (default: "parquet").
#'
#' @return The loaded SQL data as a list.
#'
#' @examples
#' load_sql_data(sql_script_paths, sql_data_paths, relevant_db_engine, TRUE, TRUE, "parquet")
load_sql_data <- function(data_scripts, data_paths, data_db_engines, compress_files, reload_from_db, only_refresh_missing, data_type="parquet"){
  # data_scripts <- sql_script_paths
  # data_paths <- sql_data_paths
  # data_db_engines <- relevant_db_engine
  # compress_files <- compress_files
  # only_refresh_missing <- only_reload_missing_files
  # reload_from_db <- reload_data_from_sql_database
  # data_type <- "parquet"
  sql_data <- list()
  dataset_names <- names(data_paths)
  reload_all_files <- !only_refresh_missing
  if (reload_from_db){
    for (dataset_name in dataset_names){
      current_dataset_not_on_disk <- (!file.exists(data_paths[[dataset_name]]))
      
  		if ((reload_all_files) | current_dataset_not_on_disk){
  		  futile.logger::flog.info(paste("Loading in the new datasets: ", dataset_name, " from database"))
  			sql_data[[dataset_name]] <- load_from_database(dataset_name, data_scripts, data_paths, data_db_engines, compress_files)
  		} else {
  		  futile.logger::flog.info(paste("Loading in the new datasets: ", dataset_name, " from disk"))
  			sql_data[[dataset_name]] <- load_from_disk(dataset_name, data_paths, compress_files, data_type=data_type)
  		}
    }
  } else {
    for (dataset_name in dataset_names){
      futile.logger::flog.info(paste("Loading in the new datasets: ", dataset_name, " from disk"))
      sql_data[[dataset_name]] <- load_from_disk(dataset_name, data_paths, compress_files, data_type=data_type)
    }
  }
  
  return(sql_data)
}
#######################################################################
# Any additional functions relevant to the current, specific loading
#######################################################################