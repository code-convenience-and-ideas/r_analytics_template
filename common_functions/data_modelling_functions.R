#######################################################################
#
#######################################################################
# Logger setup
data_analysis_func_logger <- lgr::get_logger_glue('root/analytics/functions/data_analysis')

data_analysis_func_logger$debug('Entering data analysis functions')

#######################################################################
# Heavily reused and shared functions
#######################################################################
#' Escape spaces in variable names with backticks
#' 
#' This function escapes spaces in variable names with backticks, so that they can be used in formulas and other R code without generating errors. 
#' 
#' @param str_vec A character vector of variable names to escape. 
#' 
#' @return A character vector of variable names with spaces replaced by backticks.
#' 
#' @examples
#' escape_spaced_var(c("my var", "another var"))
#' # returns c("`my var`", "`another var`")
#'
#' my_data <- data.frame("my var" = 1:10, "another var" = 11:20)
#' formula_builder("my var", c("another var"), is_surv = FALSE, is_add = FALSE)
#' # returns "`my var` ~ `another var`"
#'
#' my_formula <- formula_builder("Surv(time, status)", c("var1", "var 2"), is_surv = TRUE, is_add = FALSE, censor_var = "censor")
#' coxph(my_formula, data = my_data)
#'
#' col_var_renamers(c("my var", "another var"))
#' # returns c("my_var", "another_var")
escape_spaced_var <- function(str_vec){
  any_white_space <- str_detect(str_vec, '\\s+')
  added_quotes <- paste('`', str_vec, '`', sep="")
  output_vec <- str_vec
  output_vec[any_white_space] <- added_quotes[any_white_space]
  return(output_vec)
}

#' Build a formula string with given y_var, x_vars, censor_var and survival status
#'
#' This function takes as input a y_var, a vector of x_vars, and some additional parameters to create a formula string for use in R functions. The output formula string is formatted in a way that is appropriate for various modeling functions in R, including both linear and survival models.
#'
#' @param y_var A string indicating the name of the dependent variable
#' @param x_vars A vector of strings indicating the names of the independent variables
#' @param is_surv A boolean indicating if the model is a survival model (default FALSE)
#' @param is_add A boolean indicating if the model should add the censoring variable as an independent variable (default FALSE)
#' @param censor_var A string indicating the name of the censoring variable for survival models (default "")
#'
#' @return A formula string in R language
#'
#' @examples
#' formula_builder("y", c("x1", "x2", "x3"))
#' formula_builder("time", c("x1", "x2", "x3"), is_surv = TRUE, censor_var = "status")
#' formula_builder("y", c("x1", "x2", "x3"), is_add = TRUE, censor_var = "z")
#'
formula_builder <- function(y_var, x_vars, is_surv=FALSE, is_add=FALSE, censor_var=""){
  dependent_var_side <- paste(escape_spaced_var(x_vars), collapse=" + ")
  if (is_surv & !is_add){
    censor_chunk <- ifelse(censor_var=="", "", paste(", ",  escape_spaced_var(censor_var), sep=""))
    independent_var_side <- paste("Surv( ", escape_spaced_var(y_var), censor_chunk, " ) ~ ", sep="")
  } else if (is_surv & is_add) {
    independent_var_side <- paste(escape_spaced_var(y_var), " + ",  escape_spaced_var(censor_var), " ~ ", sep="")
  } else {
    independent_var_side <- paste(escape_spaced_var(y_var), " ~ ", sep="")    
  }
  formula_char <- paste(independent_var_side, dependent_var_side, sep="")
  return(formula_char)
}

#' Replace Spaces in Column Names with Underscores
#'
#' This function takes a vector of column names and replaces any white spaces with underscores.
#'
#' @param str_vec a character vector of column names
#'
#' @return a character vector of column names with white spaces replaced with underscores
#'
#' @examples
#' col_var_renamers(c("column 1", "column 2", "column 3"))
#' # Returns:
#' # [1] "column_1" "column_2" "column_3"
col_var_renamers <- function(str_vec){
  stringr::str_replace_all(str_vec, "\\s+", "_")
}

getLinearModelFactors <- function(model_xlevels){
  lapply(names(model_xlevels),
         function(x) tidyr::crossing(term=model_xlevels[[x]],
                                     currentVar=x)) %>% dplyr::bind_rows()
}

getLinearModelNonFactors <- function(modelDataClasses){
  non_level_variables <- names(modelDataClasses[modelDataClasses == 'numeric'])
  
  numeric_var_df <- tibble(term=non_level_variables, currentVar=non_level_variables)
  
  return(numeric_var_df)
}

getLinearModelAllFactors <- function(linearModelType){
  dataclassVector <- attr(linearModelType$terms, 'dataClasses')[attr(linearModelType$terms, 'term.labels')]
    
  combinedModelFactors <-   bind_rows(getLinearModelFactors(linearModelType$xlevels),
                                      getLinearModelNonFactors(dataclassVector)) %>%
    dplyr::mutate(mergeTerm=if_else(term==currentVar, term, paste0(currentVar, term, sep="")))
  
  return(combinedModelFactors)
}

# Function to process dataset and merge parameters
processAndMergeParameters <- function(modelData, modelFormula, fitFunction, extractFunction, summaryFunction, filterFunction){
  # Get dependent and response variables from the formula
  dependentVars <- all.vars(modelFormula)[-1]  # Exclude the first variable, assumed to be the response variable
  responseVar <- all.vars(modelFormula)[1]  # Assume the first variable is the response variable
  
  # Fit the linear model to the entire formula and dataset first and extract parameters
  fullModel <- fitFunction(modelFormula,
                           data = modelData)

  fullModelFactors <- getLinearModelAllFactors(fullModel)
  
  fullParameters <- extractFunction(fullModel) %>%
    dplyr::rename("mergeTerm"="term")
  
  fullParameters <- fullModelFactors %>%
    dplyr::full_join(fullParameters,
                     by=c("mergeTerm"="mergeTerm"))
  
  # Filter the initial dataset using the filter function
  filteredData <- filterFunction(modelData)
  
  # Create a list to store parameter values
  parameterList <- list()
  
  
  # Loop over dependent variables
  for (depVar in dependentVars) {
    depVarSym <- sym(depVar)
    
    # Define a new formula with just the current response and dependent variable
    newFormula <- reformulate(depVar, responseVar)
    
    # Fit the model on the filtered dataset with the new formula
    newModel <- fitFunction(newFormula, data = filteredData)
    
    newModelFactors <- getLinearModelAllFactors(newModel)
    
    # Extract the model's parameters
    newParameters <- extractFunction(newModel) %>%
      dplyr::rename("mergeTerm"="term")
    
    #Check if the current column is numeric
    if (!is.numeric(filteredData[[depVar]])){
      
      # Apply the summary function to the filtered dataset and left join it into the parameter dataset
      summaryResult <- summaryFunction(filteredData %>%
                                         dplyr::group_by(dplyr::across(dplyr::all_of(depVar)))) %>%
        dplyr::mutate(mergeTerm=paste0(depVar, !!depVarSym, sep="")) %>%
        dplyr::select(-!!depVarSym)
      
    } else {
      # Apply the summary function to the filtered dataset and left join it into the parameter dataset
      summaryResult <- summaryFunction(filteredData) %>%
        dplyr::mutate(mergeTerm=depVar)
    }
    
    newParameters <- newModelFactors %>%
      dplyr::left_join(newParameters, by=c('mergeTerm')) %>%
      dplyr::left_join(summaryResult, by = c("mergeTerm"))
    
    # Add a ModelType column with value matching the dependent variable
    newParameters$currentVar <- depVar
    newParameters <- newParameters %>% relocate(term, currentVar)
    
    # Store the parameter results into the list
    parameterList[[depVar]] <- newParameters
    
  }
  
  # Bind together the rows of all the independent datasets
  mergedParameters <- bind_rows(parameterList)
  
  # Append "full" to the column names of the first dataset
  colnames(fullParameters)[-c(1, 2)] <- paste0("full_", colnames(fullParameters)[-c(1, 2)])
  
  # Left join the first dataset into the merged dataset
  mergedParameters <- left_join(mergedParameters, fullParameters,
                                by = c("term" = "term", "currentVar"='currentVar')) %>%
    dplyr::relocate(currentVar, term, n_records)
  
  return(mergedParameters)
}


#######################################################################
# Any additional functions relevant to the current, specific analysis
#######################################################################