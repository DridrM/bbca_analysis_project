library(here)

# Load local modules----
source(here("R", "load_and_clean_data.R"))

# Call the load_and_clean_csv function----
data_path <- "data/bbca_data.csv"
variables_path <- "data/variables_analysis_1.csv"
bbca_df <- load_and_clean_csv(data_path, variables_path, ",", T, "UTF-8")
