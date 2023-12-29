########################################################################################
# Description: A single line summary of this file and its purposed                   #
########################################################################################

source(file.path(main_scripts_path, 'common_functions', "data_modelling_functions.R"))
source(file.path(main_scripts_path, 'common_functions', "data_modelling_new_models.R"))

########################################################################################
# Extract key reused variables and the logger
########################################################################################

source(file.path(main_scripts_path, "00_key_variables.R"))

data_model_logger <- lgr::get_logger_glue('root/analytics/work/data_model')

data_model_logger$debug('Entering data loading script')

########################################################################################
# Define metrics for prediction and evaluation of model + model options                #
########################################################################################
data_model_logger$info('Defining metrics and metric sets')

regression_metric_set <- yardstick::metric_set(yardstick::mae, yardstick::rmse,
                                               yardstick::rsq, yardstick::mape,
                                               yardstick::msd)

binary_metric_set <- yardstick::metric_set(yardstick::average_precision, yardstick::roc_auc,
                                           yardstick::mn_log_loss, yardstick::accuracy,
                                           yardstick::sens, yardstick::precision,
                                           yardstick::kap, yardstick::recall,
                                           yardstick::precision)

prob_metrics <- yardstick::metric_set(yardstick::average_precision, yardstick::roc_auc,
                                      yardstick::mn_log_loss)

class_metrics <- yardstick::metric_set(yardstick::accuracy,
                                       yardstick::sens, yardstick::precision,
                                       yardstick::kap, yardstick::recall,
                                       yardstick::precision)

data_model_logger$info('Finished defining metrics and metric sets')

########################################################################################
# Register multiple cores for parallelism
########################################################################################
# By default, don't register the multipel cores
# all_cores <- parallel::detectCores(logical = FALSE)
#
# cl <- makePSOCKcluster(all_cores)
# registerDoParallel(cl)
# registerDoSEQ() - # register serial backend

########################################################################################
# Preparing data pipelines for models + formula and targets#
########################################################################################

########################################################################################
# Prepare model workflows + workflowsets in #
########################################################################################

########################################################################################
# Fitting the models and evaluating them											   #
########################################################################################

########################################################################################
# Explanation and investigation of predictions										   #
########################################################################################


data_model_logger$debug('Leaving data loading script')
