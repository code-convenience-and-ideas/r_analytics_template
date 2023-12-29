########################################################################################
# Manage imports and needed packages using renv for initial setup
########################################################################################
# Defined a list of relevant packages for this project
relevant_packages_list <- c(
  "vctrs", # Underlying R vctrs package other depend on. Load early for updates
  "rlang", # Underlying R language package for tidyverse. Load early for updates
  "dplyr", # core tidyverse tibble definition and manipulation
  "tidyr", # Certain extra data manipulation
  "ggplot2", # Key and core plotting library
  "cowplot", # Fantastic and core to merged plotting setups
  "odbc", # R ODBC driver interface to connect to sql servers
  "DBI", # Interface to read and write from the database over a connector
  "stringr", # Common and useful string manipulations
  "lubridate", # Common and useful data manipulations
  "keyring", # Storing and accessing secrets in OS keyring
  "arrow", # Read / write apache parquet format - fast binary columnar storage
  "testthat", # Core testing package to enable good unit tests
  "mockr", # Give ability to mock up functions in unit tests
  "covr", # Code coverage calculations for testing package test completeness
  "here", # Uses rproject file or other information to spot root dir for work
  "lgr" # Allows hierarchical loggers and similar approach to python logging
  # 'crosstalk',  # For Making Widgets interact in Rshiny work
  # 'plotly', # Allows creation of interactive html graphics
  # 'ggpubr',  # Some nice aggregate, publication quality plots
  # 'data.table',  # Used for blazing fast data operations when needed
  # 'R.utils',  # Accesses gzip and some other utilities
  # 'gam',  # Generalised additive models
  # 'yardstick', # Measure definitions for variable models
  # 'doParallel',  # Used for registering multiple cores for parallel work
  # 'english'  # Human readable ordinal names and more
)


# renv used for controls and package managements
# you want renv and pak already installed if possible
install.packages("renv")
library("renv")
library("pak")

options(env.config.pak.enabled = TRUE)

# Initial renv virtual virtual environment by default
renv::init()

# Use renv to install and cache packages
renv::install(
  relevant_packages_list,
  prompt = FALSE # Go on automatically
)

# After installing the above packages, update the lockfile and get going
renv::snapshot()

# Load in the packages
lapply(relevant_packages_list, library, character.only = TRUE)

# # Activate the renv project so it loads properly - should generally be called by renv:init so likely unneeded
# renv::activate()
