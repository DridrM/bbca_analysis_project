library(dplyr)


# Load a csv file into a dataframe, remove duplicates, select variables given
# a list of variable names, rename these variables given a list of new variable
# names. The list of old and new variable names are stored inside a csv file
# accessed via the var_path argument. Remove rows where all variables are Na.
load_and_clean_csv <- function(data_path, var_path, sep, header, encode) {
  # Load data
  dtf <- read.csv(data_path, sep = sep, header = header, fileEncoding = encode)
  
  # Load variable names
  variables <- read.csv(var_path, sep = sep, header = F, fileEncoding = encode)
  
  # Extract old variable names and new variable names from variables
  old_variables <- variables[, 2]
  new_variables <- variables[, 1]
  
  # Remove duplicates
  dtf <- dtf %>% distinct(Index, .keep_all = T)
  
  # Select variables given a vector of names
  dtf <- dtf %>% select(any_of(old_variables))
  
  # Rename with new variable names
  if (length(old_variables) != length(new_variables)) {
    stop("Error: The vectors 'old_variables' and 'new_variables' must be of the same length.")
  }
  dtf <- dtf %>% rename(!!!setNames(old_variables, new_variables))
  
  # Remove rows where all variables are Na values
  dtf <- dtf %>% filter_all(any_vars(!is.na(.)))
  
  return(dtf)
}