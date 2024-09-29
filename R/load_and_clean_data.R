library(dplyr)
library(tidyr)

# Load a csv file into a dataframe, remove duplicates, select variables given
# a list of variable names, rename these variables given a list of new variable
# names. The list of old and new variable names are stored inside a csv file
# accessed via the var_path argument. Remove rows where all variables are Na.
# 
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


# This function filters a dataframe based on filtering conditions specified in a CSV file.
# The CSV contains two columns: `column_name` (the name of the dataframe column to filter)
# and `condition` (a logical condition that will be applied to the column). The function 
# reads the CSV, iterates over each row, and applies the conditions to the dataframe 
# iteratively. The separator for the CSV file can be specified as an argument.
# 
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


# This function removes rows from a dataframe that contain NA values in the specified columns.
# It takes two arguments: 
# 1. `df`: the dataframe from which NA rows will be removed.
# 2. `column_names`: a vector of column names (as strings) where NA values will be checked.
# 
# For each column in the `column_names` vector, the function iterates over the dataframe, 
# applying a filter to remove rows where the column contains NA values.
# The function dynamically references column names using dplyr's tidy evaluation with `!!sym(column)` 
# to ensure the correct column is being filtered.
# It returns a dataframe with rows removed wherever NA values were present in the specified columns.
#
remove_na_by_columns <- function(df, column_names) {
  # Iterate over the column vector and remove row that contain Na in the current column
  for (column in column_names) {
    df <- df %>% filter(!is.na(!!sym(column)))
  }
  
  return(df)
}


# gather_group_by_summarize: This function reshapes a dataframe from wide to long format, 
# groups it by specified columns, and applies one or more summary functions to the grouped data.
#
# Parameters:
# - df: The input dataframe that contains the data to be reshaped and summarized.
# - category: The name of the new column that will hold the category names (i.e., the former column names).
# - value: The name of the new column that will hold the gathered values from the reshaped data.
# - column_span: A selection of columns (e.g., column range, or a selection helper like starts_with) 
#   that should be reshaped from wide to long format.
# - group_by_cols: A vector of column names by which the data will be grouped. 
#   These columns will be used as the grouping keys for summarization.
# - summary_funcs: A named list of functions to apply to the gathered values. 
#   The names in the list will determine the names of the new summarized columns (e.g., mean, sum, etc.).
#
# The function reshapes the data using `pivot_longer()`, then groups the dataframe 
# by the specified columns. It applies multiple summary functions (mean, sum, etc.) 
# to the gathered value column. The output is a summarized dataframe where each function 
# produces a new column containing the computed statistic.
#
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