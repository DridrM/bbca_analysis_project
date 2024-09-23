library(dplyr)

# Define a function to load data given a path----
load_and_clean_data <- function(path, sep, header, encode, variables, new_variables) {
  # Load data
  dtf <- read.csv(path, sep = sep, header = header, fileEncoding = encode)
  
  # Remove duplicates
  dtf <- dtf %>% distinct(Index, .keep_all = T)
  
  # Filter variables given a vector of names
  dtf <- dtf %>% select(any_of(variables))
  
  # Rename with new variable names
  if (length(variables) != length(new_variables)) {
    stop("Error: The vectors 'variables' and 'new_variables' must be of the same length.")
  }
  dtf <- dtf %>% rename(!!!setNames(variables, new_variables))
  
  return(dtf)
}