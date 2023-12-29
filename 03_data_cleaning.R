########################################################################################
# Description: A single line summary of the file and its purpose                   #
########################################################################################

source(file.path(work_dir, 'common_functions', "data_cleaning_functions.R"))

########################################################################################
# Extract key reused variables
########################################################################################

source(file.path(main_scripts_path, "00_key_variables.R"))

data_clean_logger <- lgr::get_logger_glue('root/analytics/work/data_cleaning')
data_clean_logger$debug('Entered data cleaning script')

########################################################################################
# Any initial additional columns and cleaning
########################################################################################


data_clean_logger$debug('Leaving data cleaning script')
