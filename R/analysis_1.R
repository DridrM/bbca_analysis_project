library(here)
library(tidyr)
library(ggplot2)

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

# Transform the df into a tidy df, group by category of impact and building type,
# summarize the values of impacts by the mean and standard deviation----
category <- "impact_type"
value <- "impact_c"
column_span <- c("contrib_chantier", "contrib_eau", "contrib_energie", "contrib_pce")
group_by_cols <- c("impact_type", "type_batiment")
summary_funcs <- list(mean = mean, std = sd)
bbca_df_summarised <- gather_group_by_summarise(bbca_df, 
                                                category, 
                                                value, 
                                                column_span, 
                                                group_by_cols, 
                                                summary_funcs)

# Create a tidy bbca_df to plot histograms----
bbca_df_tidy <- bbca_df %>% pivot_longer(cols = all_of(column_span), 
                                         names_to = c(category), 
                                         values_to = c(value))

# Plot histogram with KDE for each of the impact category----
hue_category <- "type_batiment"

# Histogram + KDE plot
ggplot(bbca_df_tidy, aes(x = !!sym(value))) + 
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black", alpha = 0.5) +
  geom_density(color = "red", linewidth = 1, alpha = 0.5) +
  facet_wrap(as.formula(paste("~", category)), scales = "free") + 
  theme_minimal()

# KDE hue by the hue_category variable
ggplot(bbca_df_tidy, aes(x = !!sym(value), fill = !!sym(hue_category))) +
  geom_density(aes(color = !!sym(hue_category)), color = "red", linewidth = 1, alpha = 0.5) +
  facet_wrap(as.formula(paste("~", category)), scales = "free") + 
  theme_minimal()

# Histogram hue by the hue_category variable
ggplot(bbca_df_tidy, aes(x = !!sym(value), fill = !!sym(hue_category))) + 
  geom_histogram(aes(y = ..density..), color = "black", alpha = 0.5, position = "identity") +
  facet_wrap(as.formula(paste("~", category)), scales = "free") + 
  theme_minimal()

# Use the summarised df to plot the repartition of impact type by building types----
ggplot(bbca_df_summarised, aes(x = type_batiment, y = mean_impact_c, fill = impact_type)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal()
