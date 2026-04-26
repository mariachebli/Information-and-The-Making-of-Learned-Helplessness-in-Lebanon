# =============================================================================
# 03_validity_analyses.R – Incremental validity, distinct prediction, uprising, vignette
# =============================================================================
#
# GOAL: Run validity and exploratory models: (1) incremental validity (cognitive
#       and emotional subscales predicting collective action intent), (2) distinct
#       prediction (info-environment predictors -> cognitive vs emotional score),
#       (3) uprising participation (logistic), (4) vignette by condition (if
#       multiple conditions). Save key results to RDS for 04_export_tables.
#
# INSTRUCTIONS:
#   1. Run 02_reliability_and_factor_structure.R first.
#   2. Working directory = project folder.
#   3. Run this script. Output: output/learned_helplessness_key_results.rds
#
# INPUT:  output/learned_helplessness_analysis_data.csv
# OUTPUT: output/learned_helplessness_key_results.rds
# =============================================================================

if (file.exists("00_config.R")) source("00_config.R")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

library(dplyr)

analysis_path <- file.path(output_dir, "learned_helplessness_analysis_data.csv")
if (!file.exists(analysis_path)) stop("Run 02 first. Missing: ", analysis_path)

dat_main <- read.csv(analysis_path, stringsAsFactors = FALSE)
message("Loaded analysis data: ", nrow(dat_main), " rows")

pred_vars <- c("info_availability", "info_credibility", "misinfo_exposure", "crisis_normalizing", "media_consumption")
pred_vars <- pred_vars[pred_vars %in% names(dat_main)]
# Use only predictors with enough non-NA values (misinfo_exposure may be absent in new export)
pred_vars_use <- pred_vars[sapply(dat_main[pred_vars], function(x) sum(!is.na(x))) >= 20]
key_results <- list()

# -----------------------------------------------------------------------------
# 1. Reliability table (from 02 – re-read or pass; we store summary here for 04)
# -----------------------------------------------------------------------------
r02 <- readRDS(file.path(output_dir, "02_reliability_factor_results.rds"))
key_results$reliability <- data.frame(
  scale = c("cognitive", "emotional"),
  alpha = c(r02$alpha_cognitive, r02$alpha_emotional)
)

# -----------------------------------------------------------------------------
# 2. Incremental validity: collective_action_intent ~ cog, emo, both
# -----------------------------------------------------------------------------
valid_rows <- dat_main %>% filter(!is.na(collective_action_intent))
if (nrow(valid_rows) >= 10) {
  mod_cog_only <- lm(collective_action_intent ~ cognitive_score, data = valid_rows)
  mod_emo_only <- lm(collective_action_intent ~ emotional_score, data = valid_rows)
  mod_both <- lm(collective_action_intent ~ cognitive_score + emotional_score, data = valid_rows)
  message("Incremental validity (N = ", nrow(valid_rows), "):")
  summary(mod_cog_only)
  summary(mod_emo_only)
  summary(mod_both)
  key_results$incremental_validity <- data.frame(
    model = c("cog_only", "emo_only", "both"),
    R2 = c(summary(mod_cog_only)$r.squared, summary(mod_emo_only)$r.squared, summary(mod_both)$r.squared)
  )
} else {
  message("Insufficient non-missing collective_action_intent; skipping incremental validity.")
  key_results$incremental_validity <- NULL
}

# -----------------------------------------------------------------------------
# 3. Distinct prediction: info-environment -> cognitive_score, emotional_score
# -----------------------------------------------------------------------------
if (length(pred_vars_use) >= 2) {
  valid_info <- dat_main %>% filter(complete.cases(across(all_of(pred_vars_use))))
} else {
  valid_info <- dat_main[0, ]
}
if (nrow(valid_info) >= 20) {
  form_cog <- as.formula(paste("cognitive_score ~", paste(pred_vars_use, collapse = " + ")))
  form_emo <- as.formula(paste("emotional_score ~", paste(pred_vars_use, collapse = " + ")))
  mod_cog_info <- lm(form_cog, data = valid_info)
  mod_emo_info <- lm(form_emo, data = valid_info)
  message("Distinct prediction (N = ", nrow(valid_info), "):")
  summary(mod_cog_info)
  summary(mod_emo_info)
  key_results$info_to_helplessness <- list(cognitive = summary(mod_cog_info)$coefficients, emotional = summary(mod_emo_info)$coefficients)
} else {
  message("Insufficient complete info-environment data; skipping distinct prediction.")
  key_results$info_to_helplessness <- NULL
}

# -----------------------------------------------------------------------------
# 4. Uprising participation (logistic)
# -----------------------------------------------------------------------------
if ("uprising_participated_binary" %in% names(dat_main)) {
  upr_data <- dat_main %>% filter(!is.na(uprising_participated_binary))
  if (length(pred_vars_use) >= 2) {
    upr_data <- upr_data %>% filter(complete.cases(across(all_of(pred_vars_use))))
  }
  if (nrow(upr_data) >= 20) {
    form_upr <- as.formula(paste("uprising_participated_binary ~", paste(pred_vars_use, collapse = " + ")))
    mod_uprising <- glm(form_upr, data = upr_data, family = binomial)
    summary(mod_uprising)
    coefs <- summary(mod_uprising)$coefficients
    key_results$uprising_participation <- data.frame(term = rownames(coefs), OR = exp(coefs[, "Estimate"]), p = coefs[, "Pr(>|z|)"])
  } else {
    key_results$uprising_participation <- NULL
  }
} else {
  key_results$uprising_participation <- NULL
}

# -----------------------------------------------------------------------------
# 5. Vignette means by condition (if 2+ conditions)
# -----------------------------------------------------------------------------
if ("vignette_condition" %in% names(dat_main)) {
  vdat <- dat_main %>% filter(!is.na(vignette_condition))
  n_cond <- length(unique(vdat$vignette_condition))
  if (nrow(vdat) > 0 && n_cond >= 2) {
    vignette_means <- vdat %>%
      group_by(vignette_condition) %>%
      summarise(
        n = n(),
        mean_understanding = mean(v_understanding, na.rm = TRUE),
        mean_likely_change = mean(v_likely_change, na.rm = TRUE),
        mean_protest_intent = mean(v_protest_intent, na.rm = TRUE),
        mean_hope = mean(v_hope, na.rm = TRUE),
        mean_tired = mean(v_tired, na.rm = TRUE),
        .groups = "drop"
      )
    key_results$vignette_condition_means <- as.data.frame(vignette_means)
  } else {
    key_results$vignette_condition_means <- NULL
  }
} else {
  key_results$vignette_condition_means <- NULL
}

saveRDS(key_results, file.path(output_dir, "learned_helplessness_key_results.rds"))
message("Saved: ", file.path(output_dir, "learned_helplessness_key_results.rds"))


print(summary(mod_cog_only))
print(summary(mod_emo_only))
print(summary(mod_both))
print(summary(mod_cog_info))
print(summary(mod_emo_info))
print(summary(mod_uprising))