########################################################################################
# Description: A single line summary of the file and its purposed                   #
########################################################################################
source(file.path(main_scripts_path, 'common_functions', "data_loading_functions.R"))

########################################################################################
# Extract key reused variables and the logger
########################################################################################

source(file.path(main_scripts_path, "00_key_variables.R"))

data_load_logger <- lgr::get_logger_glue('root/analytics/work/data_loading')
data_load_logger$debug('Entering data loading script')
########################################################################################
# Loads in the database connections and data coming from disk for database             #
########################################################################################

# Establish database connections for pulling in data                                   #
data_load_logger$info("Establishing database connections", name = "standard_logger")

# Database connection
my_db_name <- "current_db"

# Get your username from keyring dynamically to avoid script update
# my_db_username <- key_get(my_db_name, 'user')  #  "current_db_username"

# test_database_credentials(odbc::odbc(),
#                           driver = "Oracle in OraDB12Home1",
#                           server = my_db_name,
#                           dbq = my_db_name,
#                           uid = my_db_username,
#                           pwd = key_get(my_db_name, my_db_username))

# database_connection = dbConnect(
#     odbc::odbc(),
#     driver = "Oracle in OraDB12Home1",
#     server = my_db_name,
#     dbq = my_db_name,
#     uid = my_db_username,
#     pwd = key_get(my_db_name, my_db_username)  #Pull the password for that user in that db from keyring
# )

data_load_logger$info("Successfully finished connecting to the databases")

########################################################################################
# Load data in from the databases                                                      #
########################################################################################

compress_files <- FALSE
reload_data_from_sql_database <- TRUE
only_reload_missing_files <- FALSE

# Make a set of dataset names for consistent labelling
generic_1 <- "generic_1"

dataset_names <- c(generic_1)

sql_script_paths <- setNames(as.list(c(
  generic_1 = file.path(sql_scripts_dir, "generic_sql_script.sql")
)), dataset_names)

sql_data_paths <- setNames(as.list(c(
  generic_1 = file.path(raw_data_dir, "sql_data.parquet")
)), dataset_names)
#
# relevant_db_engine <- setNames(as.list(c(
#   generic_1 = database_connection
# )), dataset_names)


# sql_data <- load_sql_data(sql_script_paths, sql_data_paths, relevant_db_engine, compress_files, reload_data_from_sql_database, only_reload_missing_files, data_type="parquet")

########################################################################################
# Load data from disk                                                                  #
########################################################################################
load_disk_data <- function(){
  disk_data <- list()
  # disk dataset 1

  # Clean up no longer needed data for producing disk dataset 1

  # disk dataset 2

  # Clean up no longer needed data for producing disk dataset 2

  return(disk_data)
}

disk_data <- load_disk_data()

data_load_logger$debug('Leaving data loading script')
