# =============================================================================
# 05_mediation_example.R – Mediation: info environment → LPH → collective action
# =============================================================================
# Run after 01–04 so learned_helplessness_analysis_data.csv exists.
# Path model: IV → M → DV, plus direct IV → DV. Indirect = a*b, total = c + a*b.
# Default: IV = info_availability, M = combined_helplessness, DV = collective_action_intent.
# Try crisis_normalizing as IV if you want a stronger a path (see 06 output).
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(lavaan)

# Read the analysis dataset (from 02)
analysis_path <- file.path(output_dir, "learned_helplessness_analysis_data.csv")
if (!file.exists(analysis_path)) stop("Run 01_cleaning.R and 02_reliability_and_factor_structure.R first. Missing: ", analysis_path)

dat <- read.csv(analysis_path, stringsAsFactors = FALSE)

# Use complete cases on IV, M, DV
dat <- dat[complete.cases(dat[, c("info_availability", "combined_helplessness", "collective_action_intent")]), ]
message("Mediation example N = ", nrow(dat))

# ---- Path model in lavaan ----
# a path: IV -> M
# b path: M -> DV (controlling IV)
# c path: IV -> DV (direct effect)
# Indirect effect = a*b; total effect = c + (a*b)
model_med <- "
  # Regressions
  combined_helplessness ~ a * info_availability
  collective_action_intent ~ b * combined_helplessness + c * info_availability
  # Indirect effect (a*b) and total effect (c + a*b)
  indirect := a * b
  total    := c + (a * b)
"

fit_med <- sem(model_med, data = dat, fixed.x = FALSE)
message("\n--- Mediation model: info_availability -> combined_helplessness -> collective_action_intent ---")
summary(fit_med, standardized = TRUE, rsq = TRUE)

# Optional: bootstrap CI for indirect effect (run if you want CIs)
# fit_med_boot <- sem(model_med, data = dat, fixed.x = FALSE, se = "bootstrap", bootstrap = 2000)
# parameterEstimates(fit_med_boot, boot.ci.type = "perc")[parameterEstimates(fit_med_boot)$label == "indirect", ]

# ---- Alternative: two mediators (cognitive and emotional) ----
# Uncomment and run if you want both subscales as mediators.
# model_med2 <- "
#   cognitive_score ~ a1 * info_availability
#   emotional_score ~ a2 * info_availability
#   collective_action_intent ~ b1 * cognitive_score + b2 * emotional_score + c * info_availability
#   indirect_cog := a1 * b1
#   indirect_emo := a2 * b2
#   indirect_total := (a1 * b1) + (a2 * b2)
#   total := c + indirect_total
# "
# dat2 <- dat[complete.cases(dat[, c("info_availability", "cognitive_score", "emotional_score", "collective_action_intent")]), ]
# fit_med2 <- sem(model_med2, data = dat2, fixed.x = FALSE)
# summary(fit_med2, standardized = TRUE, rsq = TRUE)

message("\nDone. Adapt the IV (e.g. info_credibility, crisis_normalizing) or add controls as needed for your thesis.")
