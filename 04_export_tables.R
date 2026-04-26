# =============================================================================
# 04_export_tables.R – Export final indices CSV and summary tables
# =============================================================================
#
# GOAL: Read the analysis dataset and key results, then write (1) the final
#       indices CSV for Maria (one row per respondent, key variables) and
#       (2) any summary tables useful for the thesis or reporting.
#
# INSTRUCTIONS:
#   1. Run 01, 02, and 03 first.
#   2. Working directory = project folder.
#   3. Run this script. Outputs: output/learned_helplessness_indices.csv,
#      and optionally output/04_summary_tables.txt or extra CSVs.
#
# INPUT:  output/learned_helplessness_analysis_data.csv, output/learned_helplessness_key_results.rds
# OUTPUT: output/learned_helplessness_indices.csv
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(dplyr)

analysis_path <- file.path(output_dir, "learned_helplessness_analysis_data.csv")
results_path <- file.path(output_dir, "learned_helplessness_key_results.rds")
if (!file.exists(analysis_path)) stop("Run 02 first. Missing: ", analysis_path)
if (!file.exists(results_path)) stop("Run 03 first. Missing: ", results_path)

dat_main <- read.csv(analysis_path, stringsAsFactors = FALSE)
key_results <- readRDS(results_path)

cog_cols <- paste0("Q24_", 1:5)
emo_cols <- paste0("Q26_", 1:6)

# Columns to include in the indices CSV (one row per respondent)
index_vars <- c(
  "ResponseId",
  cog_cols, emo_cols,
  "cognitive_score", "emotional_score", "combined_helplessness", "pca1_score",
  "info_availability", "misinfo_exposure", "crisis_normalizing", "media_consumption",
  "collective_action_intent",
  "uprising_participated_binary",
  "vignette_condition",
  "v_understanding", "v_likely_change", "v_protest_intent", "v_hope", "v_anger", "v_tired", "v_confused"
)

indices_table <- dat_main %>% select(any_of(index_vars))
write.csv(indices_table, file.path(output_dir, "learned_helplessness_indices.csv"), row.names = FALSE)
message("Saved: ", file.path(output_dir, "learned_helplessness_indices.csv"))

# Optional: write a short summary text for quick reference
sink(file.path(output_dir, "04_analysis_summary.txt"))
cat("Learned Political Helplessness – analysis summary\n")
cat("=================================================\n\n")
cat("Reliability:\n")
if (!is.null(key_results$reliability)) print(key_results$reliability)
cat("\nIncremental validity (R²):\n")
if (!is.null(key_results$incremental_validity)) print(key_results$incremental_validity)
cat("\nVignette condition means (if available):\n")
if (!is.null(key_results$vignette_condition_means)) print(key_results$vignette_condition_means)
sink()
message("Saved: ", file.path(output_dir, "04_analysis_summary.txt"))
