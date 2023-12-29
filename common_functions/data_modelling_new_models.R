#######################################################################
#
#######################################################################
# Logger setup
data_modelling_func_logger <- lgr::get_logger_glue('root/analytics/functions/data_modelling')

data_modelling_func_logger$debug('Entering data modelling functions')

#########################################################################################
# Define new tidymodels style definitions for any additional or custom models needed for test
#########################################################################################