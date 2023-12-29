#######################################################################
# Description: A single line summary of the project and its purpose
#######################################################################
# In context of an R project and using renv, packages should be sorted

#########################################################
# Set up paths                                          #
#########################################################
# Paths are aligned to my general project directory structure
## Set an overall working directory for rest of structure
## Uses here package which employs heuristics to intelligently guess work dir
work_dir <- here::here()

## various scripts locations
scripts_dir <- file.path(work_dir, "scripts")
sql_scripts_dir <- file.path(scripts_dir, "sql")
r_scripts_dir <- file.path(scripts_dir, "r")
python_scripts_dir <- file.path(scripts_dir, "python")
other_scripts_dir <- file.path(scripts_dir, "other")

## different possible data directories
data_dir <- file.path(work_dir, "data")
manual_data_dir <- file.path(data_dir, "manual_data")
processed_data_dir <- file.path(data_dir, "processed_data")
raw_data_dir <- file.path(data_dir, "raw_data")
synthesised_data_dir <- file.path(
  data_dir,
  "summaries_reports_synthesised_data"
)

## more for record keeping + method keeping
documentation_dir <- file.path(work_dir, "documentation")
logs_dir <- file.path(work_dir, "logs")

## Analytics byproducts of the work
reports_dir <- file.path(work_dir, "reports")
figures_dir <- file.path(work_dir, "figures")
models_dir <- file.path(work_dir, "models")

## Other directory
other_dir <- file.path(work_dir, "other")
