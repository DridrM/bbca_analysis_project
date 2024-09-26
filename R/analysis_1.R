library(here)

# Load local modules----
source(here("R", "load_and_clean_data.R"))

# Call the load_and_clean_csv function----
data_path <- "data/bbca_data.csv"
variables_path <- "data/variables_analysis_1.csv"
bbca_df <- load_and_clean_csv(data_path, variables_path, ",", T, "UTF-8")

# Filter the bbca_df on the pairs column name / condition inside the filter_cond_analysis_1.csv file----
filter_conditions_path <- "data/filter_cond_analysis_1.csv"
bbca_df <- filter_df_by_csv(bbca_df, ";", filter_conditions_path)

# Remove the row that contain Na in the following columns----
columns <- c("contrib_chantier", "contrib_eau", "contrib_energie", "contrib_pce")
bbca_df <- remove_na_by_columns(bbca_df, columns)
