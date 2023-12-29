########################################################################################
# Description: A single line summary of the project and its purpose                    #
# Author: Alex Baker                                                                   #
# Contact:                                              #
# Project:#
########################################################################################
# Covering two cases -> template folder structure, and, single flat folder
# Should get appropriate working dir if not defined yet
work_dir <- here::here()

# Handle cases to pick the right path for each structure
main_scripts_path <- here::here("scripts", "r")
main_scripts_path <- if (!dir.exists(main_scripts_path)) work_dir else main_scripts_path
logs_dir <- here::here("logs")
logs_dir <- if (!dir.exists(logs_dir)) work_dir else logs_dir
log_file_path <- file.path(logs_dir, 'main_logs.json')
# Use renv to load local environment if it exists in project folder
# If it does not exist, go to import.R to run and setup R virtual environment
renv::restore()

# Load configuration script
# Assumes config file is in the working directory
source(file.path(main_scripts_path, "01_configuration.R"))

# Define logger setup with extra appenders, names and thresholds
## Use root logger to define console format and shared threshold in other loggers
lgr::lgr$set_threshold("debug")

lgr::lgr$appenders$console$set_layout(lgr::LayoutGlue$new(
  fmt = paste0(c("{toupper(level_name)} ({level}) [{timestamp}]",
                 " {.logger$name} {level_name} {caller}: {msg}"),
               collapse = "")
))


core_analytics_logger <- lgr::get_logger_glue("analytics")
core_analytics_logger$add_appender(
    lgr::AppenderJson$new(file = log_file_path),
    name = "logfile"
)

# Run the import of packages needed for the project
# Set up the database connections and load in the data coming from the database / disk
core_analytics_logger$info("Loading data from the databases")
source(file.path(main_scripts_path, "02_data_loading.R"))
core_analytics_logger$info("Finished loading data")

# Clean and modify the data in any crucially needed ways for downstream analysis
core_analytics_logger$info("Cleaning data for downstream analysis")
source(file.path(main_scripts_path, "03_data_cleaning.R"))
core_analytics_logger$info("Finished cleaning data")

# Do the analysis of the data itself, exploratory, models, plots
core_analytics_logger$info("Performing analysis, visualisation, plotting etc of the data")
source(file.path(main_scripts_path, "04_data_analysis.R"))
core_analytics_logger$info("Finished data analysis")

# Do data modelling, model fitting, prediction, saving etc
core_analytics_logger$info("Performing model fitting")
source(file.path(main_scripts_path, "05_data_modelling.R"))
core_analytics_logger$info("Finished model fitting")
