# =============================================================================
# 00_config.R – Configuration for Learned Political Helplessness pipeline
# =============================================================================
# Run this script first, or source it from 01_cleaning.R.
# Change data_file and allow_preview_for_testing to match your data.
# =============================================================================

# ---- Data file ----
# Path to the Qualtrics CSV export (relative to project folder or full path).
# Current: April 7, 2026 — Labels export (live responses only; choice text, not numeric codes).
data_file <- "~/LocalWork/Consults/maria_honors/Learned Political Helplessness in Lebanon_April 7, 2026_14.47_Labels.csv"

# ---- Response inclusion ----
# TRUE  = keep finished responses even if Status is "Survey Preview" (for test exports).
# FALSE = exclude Preview/Test rows (use for real data).
allow_preview_for_testing <- FALSE

# ---- Output ----
# All scripts write to this folder: cleaned data, reports, and result tables.
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ---- Convenience ----
# So other scripts can check which file was used (for logging).
get_data_file <- function() data_file
get_output_dir <- function() output_dir

message("Config loaded: data_file = ", data_file, ", allow_preview = ", allow_preview_for_testing, ", output_dir = ", output_dir)
