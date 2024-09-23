library(here)

# Load local modules----
source(here("R", "load_and_clean_data.R"))

# Call the load_and_clean_data function----
bbca_df <- load_and_clean_data("data/bbca_data.csv", ",", T, "UTF-8")
