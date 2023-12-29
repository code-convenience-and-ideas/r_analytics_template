########################################################################################
# Description: A single line summary of the project and its purposed                   #
# Author: Alex Baker	                                                               #
# Contact: hostscodeonline@gmail.com                                              #
# Project: 																			   #
########################################################################################

source(file.path(main_scripts_path, 'common_functions', "data_analysis_functions.R"))

########################################################################################
# Extract key reused variables
########################################################################################

source(file.path(main_scripts_path, "00_key_variables.R"))

data_analysis_logger <- lgr::get_logger_glue('root/analytics/work/data_analysis')
data_analysis_logger$debug("Entered analysis script")
########################################################################################
# Initial exploratory analysis                                                         #
########################################################################################

########################################################################################
# Initial Modelling                                                                    #
########################################################################################


########################################################################################
# Final Modelling                                                                      #
########################################################################################

data_analysis_logger$debug("Leaving analysis script")
