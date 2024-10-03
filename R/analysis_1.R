library(tidyr)
library(config)
library(ggplot2)
library(BBCAproj)


# Get the default configuration
config <- config::get()

# Call the load and clean csv function----
data_path <- config$csv$data_path
variables_path <- config$csv$analysis_1$variables_path
bbca_df <- BBCAproj::load_and_clean_csv(data_path, variables_path, ",", T, "UTF-8")

# Filter the bbca_df on the pairs column name / condition inside the filter_cond_analysis_1.csv file----
filter_conditions_path <- config$csv$analysis_1$filter_conditions_path
bbca_df <- BBCAproj::filter_df_by_csv(bbca_df, ";", filter_conditions_path)

# Remove the row that contain Na in the following columns----
columns <- c("contrib_chantier", "contrib_eau", "contrib_energie", "contrib_pce")
bbca_df <- BBCAproj::remove_na_by_columns(bbca_df, columns)

# Transform the df into a tidy df, group by category of impact and building type,
# summarize the values of impacts by the mean and standard deviation----
category <- "impact_type"
value <- "impact_c"
column_span <- c("contrib_chantier", "contrib_eau", "contrib_energie", "contrib_pce")
group_by_cols <- c("impact_type", "type_batiment")
summary_funcs <- list(mean = mean, std = sd)
bbca_df_summarised <- BBCAproj::gather_group_by_summarise(bbca_df,
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
hist_kde <- ggplot(bbca_df_tidy, aes(x = !!sym(value))) +
  geom_histogram(aes(y = after_stat(density)), fill = "blue", color = "black", alpha = 0.5) +
  geom_density(color = "red", linewidth = 1, alpha = 0.5) +
  facet_wrap(as.formula(paste("~", category)), scales = "free") +
  theme_minimal()

# KDE hue by the hue_category variable
kde_hue <- ggplot(bbca_df_tidy, aes(x = !!sym(value), fill = !!sym(hue_category))) +
  geom_density(aes(color = !!sym(hue_category)), color = "red", linewidth = 1, alpha = 0.5) +
  facet_wrap(as.formula(paste("~", category)), scales = "free") +
  theme_minimal()

# Histogram hue by the hue_category variable
hist_hue <- ggplot(bbca_df_tidy, aes(x = !!sym(value), fill = !!sym(hue_category))) +
  geom_histogram(aes(y = after_stat(density)), color = "black", alpha = 0.5, position = "identity") +
  facet_wrap(as.formula(paste("~", category)), scales = "free") +
  theme_minimal()

# Violin hue by the hue_category variable
violin_hue <- ggplot(bbca_df_tidy, aes(x = !!sym(hue_category), y = !!sym(value), fill = !!sym(hue_category))) +
  geom_violin(color = "black", alpha = 0.5, position = "dodge") +
  facet_wrap(as.formula(paste("~", category)), scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Use the summarised df to plot the repartition of impact type by building types----
impact_rep <- ggplot(bbca_df_summarised, aes(x = type_batiment, y = mean_impact_c, fill = impact_type)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_minimal()

# Save the plots----
figures_path <- config$figures$analysis_1$path
if (!dir.exists(figures_path)) {
  dir.create(figures_path, recursive = T)
}

ggsave(paste(figures_path, "histograms_per_impact_type.png"), plot = hist_kde, width = 12, height = 9)
ggsave(paste(figures_path, "densities_per_impact_and_building_type.png"), plot = kde_hue, width = 12, height = 9)
ggsave(paste(figures_path, "histograms_per_impact_and_building_type.png"), plot = hist_hue, width = 12, height = 9)
ggsave(paste(figures_path, "violins_per_impact_and_building_type.png"), plot = violin_hue, width = 12, height = 9)
ggsave(paste(figures_path, "barplot_impact_type_given_building_type.png"), plot = impact_rep, width = 12, height = 9)
