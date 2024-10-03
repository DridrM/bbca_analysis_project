library(dplyr)
library(tidyr)

#' Load and Clean CSV Data
#'
#' This function loads a CSV file, selects and renames columns based on a provided variable mapping,
#' removes duplicate rows based on an index column, and filters out rows with only missing values.
#'
#' @param data_path A character string specifying the path to the CSV data file.
#' @param var_path A character string specifying the path to the CSV file containing the variable mapping.
#'   This file should have two columns: the first for the new variable names and the second for the old variable names.
#' @param sep A character string specifying the field separator character for both CSV files (e.g., `","`, `";"`).
#' @param header A logical value indicating whether the CSV files contain a header row.
#' @param encode A character string specifying the encoding of the CSV files (e.g., `"UTF-8"`, `"ISO-8859-1"`).
#'
#' @return A cleaned data frame where:
#' \describe{
#'   \item{Duplicates}{are removed based on the `Index` column.}
#'   \item{Selected columns}{are based on the old variable names provided in the variable mapping file.}
#'   \item{Columns}{are renamed based on the corresponding new variable names.}
#'   \item{Rows}{containing only missing values are removed.}
#' }
#' 
#' @import dplyr
#'
#' @export
load_and_clean_csv <- function(data_path, var_path, sep, header, encode) {
  # Load data
  df <- read.csv(data_path, sep = sep, header = header, fileEncoding = encode)
  
  # Load variable names
  variables <- read.csv(var_path, sep = sep, header = F, fileEncoding = encode)
  
  # Extract old variable names and new variable names from variables
  old_variables <- variables[, 2]
  new_variables <- variables[, 1]
  
  # Remove duplicates
  df <- df %>% distinct(Index, .keep_all = T)
  
  # Select variables given a vector of names
  df <- df %>% select(any_of(old_variables))
  
  # Rename with new variable names
  if (length(old_variables) != length(new_variables)) {
    stop("Error: The vectors 'old_variables' and 'new_variables' must be of the same length.")
  }
  df <- df %>% rename(!!!setNames(old_variables, new_variables))
  
  # Remove rows where all variables are Na values
  df <- df %>% filter_all(any_vars(!is.na(.)))
  
  return(df)
}


#' Filter a DataFrame Based on Conditions from a CSV File
#'
#' This function filters rows in a dataframe by applying conditions specified in a CSV file.
#' The CSV file should contain two columns: `column_name` and `condition`. For each row in the CSV,
#' the function applies the specified condition to the specified column and filters the dataframe accordingly.
#'
#' @param df A dataframe to be filtered.
#' @param sep A character string specifying the field separator character for the CSV file.
#' @param filter_csv_path A string containing the path to the CSV file with the filter conditions.
#'
#' @return The filtered dataframe after all conditions have been applied.
#' 
#' @import dplyr
#'
#' @export
filter_df_by_csv <- function(df, sep, filter_csv_path) {
  # Load the pairs column name / condition
  filter_conditions <- read.csv(filter_csv_path, sep = sep, header = T, 
                                stringsAsFactors = F, encoding = 'UTF-8')
  
  # Iterate through each row in the CSV and apply the filter
  for (i in seq_len(nrow(filter_conditions))) {
    column_name <- filter_conditions$column_name[i]
    condition <- filter_conditions$condition[i]
    
    # Dynamically evaluate the condition within the context of the dataframe
    df <- df %>%
      filter(eval(parse(text = condition)))
  }
  
  return(df)
}


#' Remove Rows with NA Values from Specified Columns
#'
#' This function removes rows from a dataframe that contain `NA` (missing values) in any of the specified columns.
#' The function iterates through the list of column names and removes rows that have missing values in each specified column.
#'
#' @param df A dataframe from which rows with `NA` values will be removed.
#' @param column_names A character vector containing the names of the columns to check for `NA` values.
#'
#' @return A dataframe with rows containing `NA` values removed from the specified columns.
#' 
#' @import dplyr
#'
#' @export
remove_na_by_columns <- function(df, column_names) {
  # Iterate over the column vector and remove row that contain Na in the current column
  for (column in column_names) {
    df <- df %>% filter(!is.na(!!sym(column)))
  }
  
  return(df)
}


#' Reshape, Group, and Summarise a DataFrame
#'
#' This function reshapes a dataframe from wide to long format, groups by specified columns, and applies multiple summary functions to the values in the reshaped dataframe.
#'
#' @param df A dataframe to be reshaped, grouped, and summarised.
#' @param category A string specifying the name of the new column that will hold the reshaped variable names (from wide to long format).
#' @param value A string specifying the name of the new column that will hold the reshaped values (from wide to long format).
#' @param column_span A character vector specifying the columns to reshape from wide to long format.
#' @param group_by_cols A character vector specifying the columns to group by after reshaping.
#' @param summary_funcs A named list of functions to be applied as summaries to the `value` column after grouping.
#'
#' @return A summarised dataframe with the results of the applied summary functions.
#' 
#' @import dplyr
#' @import tidyr
#'
#' @export
gather_group_by_summarise <- function(df, category, value, column_span, group_by_cols, summary_funcs) {
  # Reshape the dataframe from wide to long format using pivot_longer
  df <- df %>%
    pivot_longer(cols = all_of(column_span), names_to = c(category), values_to = c(value)) %>%
    
    # Group by the specified columns
    group_by(across(all_of(group_by_cols))) %>%
    
    # Apply multiple summary functions
    summarise(across(.cols = !!sym(value), 
                     .fns = list(!!!summary_funcs),
                     .names = "{fn}_{col}"),
              .groups = 'drop')
  
  return(df)
}