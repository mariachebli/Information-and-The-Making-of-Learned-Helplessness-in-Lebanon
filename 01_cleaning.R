# =============================================================================
# 01_cleaning.R – Read Qualtrics export, recode, build indices
# =============================================================================
# Two formats supported: (1) New = renamed cols (state_efficacy_*, etc.) with
#     either numeric codes (Values export) or choice text (Labels export).
# (2) Old = Q24_*, Q26_*, etc. with text labels. Output uses canonical names
# (Q24_*, Q26_*, ...) so 02, 03, 04 don't need to change.
# =============================================================================

if (file.exists("00_config.R")) {
  source("00_config.R")
} else {
  data_file <- "~/LocalWork/Consults/maria_honors/Learned Political Helplessness in Lebanon_March 5, 2026_19.17.csv"
  allow_preview_for_testing <- FALSE
  output_dir <- "output"
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
}

library(dplyr)

if (!file.exists(data_file)) stop("Data file not found: ", data_file)
raw <- read.csv(data_file, check.names = TRUE, stringsAsFactors = FALSE)
n_raw <- nrow(raw)
raw <- raw[-c(1, 2), ]
n_after_drop <- nrow(raw)

# Finished: "1"/"True" = completed
dat <- raw %>% filter(Finished %in% c("1", "True"))
n_finished <- nrow(dat)
if (!allow_preview_for_testing && "Status" %in% names(dat)) {
  dat <- dat %>% filter(!as.character(Status) %in% c("Survey Preview", "Survey Test", "Response Type"))
}
n_analysis_eligible <- nrow(dat)

msg <- character(0)
msg <- c(msg, "========== 01_cleaning: Data source and N ==========")
msg <- c(msg, paste("Data file:", data_file))
msg <- c(msg, paste("Rows in file (after header):", n_raw))
msg <- c(msg, paste("Rows after dropping row 1–2:", n_after_drop))
msg <- c(msg, paste("Rows with Finished completed:", n_finished))
msg <- c(msg, paste("Rows after excluding Preview/Test:", n_analysis_eligible))
message(paste(msg, collapse = "\n"))
if (n_analysis_eligible == 0) stop("No rows left. Check Finished and Status.")

# ---- Text recodes (old Q-ID format + new-format *Labels* export) ----
recode_agree4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("strongly disagree")] <- 1
  out[tolower(x) %in% c("somewhat disagree")] <- 2
  out[tolower(x) %in% c("somewhat agree")] <- 3
  out[tolower(x) %in% c("strongly agree")] <- 4
  out
}
recode_freq4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("often", "very often")] <- 1
  out[tolower(x) %in% c("sometimes")] <- 2
  out[tolower(x) %in% c("rarely")] <- 3
  out[tolower(x) %in% c("never")] <- 4
  out
}
recode_likely4 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("extremely unlikely")] <- 1
  out[tolower(x) %in% c("somewhat unlikely")] <- 2
  out[tolower(x) %in% c("somewhat likely")] <- 3
  out[tolower(x) %in% c("extremely likely")] <- 4
  out
}
recode_media_freq5 <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[tolower(x) %in% c("never")] <- 1
  out[tolower(x) %in% c("once a week")] <- 2
  out[tolower(x) %in% c("2-3 times a week", "2–3 times a week")] <- 3
  out[tolower(x) %in% c("4-6 times a week", "4–6 times a week")] <- 4
  out[tolower(x) %in% c("daily")] <- 5
  out
}

# ---- Detect format and build canonical columns (Q24_*, Q26_*, etc.) ----
new_format <- "state_efficacy_1" %in% names(dat)

if (new_format) {
  to_num <- function(x) { x <- as.character(x); x[x == ""] <- NA; suppressWarnings(as.numeric(x)) }
  cog_cols_new <- paste0("state_efficacy_", 1:5)
  emo_cols_new <- paste0("emotional_indicator_", 1:6)
  ca_cols_new  <- paste0("collec_action_likely_", 1:5)
  info_avail_new <- paste0("info_availability_", 1:4)
  info_cred_new  <- paste0("info_credibility_", 1:4)
  crisis_new     <- paste0("media_crisis_cred_", 1:3)
  tv_new    <- paste0("tv_channels_freq_", 1:9)
  news_new  <- paste0("newspaper_freq_", 1:8)
  wa_new    <- "whatsapp_freq_1"
  soc_new   <- paste0("social_media_freq_", 1:6)
  cols_to_convert <- c(cog_cols_new, emo_cols_new, ca_cols_new, info_avail_new, info_cred_new, crisis_new, tv_new, news_new, wa_new, soc_new)
  cols_to_convert <- cols_to_convert[cols_to_convert %in% names(dat)]
  # Labels export: choice text in cells → as.numeric() is NA; use same recodes as Q-ID export
  new_format_labels <- FALSE
  if (cog_cols_new[1] %in% names(dat)) {
    v0 <- trimws(as.character(dat[[cog_cols_new[1]]]))
    v0[v0 == ""] <- NA
    nn <- sum(!is.na(v0))
    prop_num <- if (nn > 0) mean(!is.na(suppressWarnings(as.numeric(v0[!is.na(v0)])))) else 0
    new_format_labels <- prop_num < 0.5
  }
  if (new_format_labels) {
    msg <- c(msg, "New-format *Labels* export (choice text): text recodes on renamed columns.")
    for (col in cog_cols_new) if (col %in% names(dat)) dat[[col]] <- recode_agree4(dat[[col]])
    for (col in emo_cols_new) if (col %in% names(dat)) dat[[col]] <- recode_freq4(dat[[col]])
    for (col in ca_cols_new) if (col %in% names(dat)) dat[[col]] <- recode_likely4(dat[[col]])
    for (col in c(info_avail_new, info_cred_new, crisis_new)) if (col %in% names(dat)) dat[[col]] <- recode_agree4(dat[[col]])
    for (col in c(tv_new, news_new, wa_new, soc_new)) if (col %in% names(dat)) dat[[col]] <- recode_media_freq5(dat[[col]])
  } else {
    for (col in cols_to_convert) dat[[col]] <- to_num(dat[[col]])
  }
  # Canonical names for downstream scripts
  cog_cols <- paste0("Q24_", 1:5)
  emo_cols <- paste0("Q26_", 1:6)
  ca_cols  <- paste0("Q31_", 1:5)
  info_avail_cols <- paste0("Q13_", 1:4)
  info_cred_cols  <- paste0("Q12_", 1:4)
  crisis_norm_cols <- paste0("Q15_", 1:3)
  for (i in 1:5) dat[[paste0("Q24_", i)]] <- dat[[cog_cols_new[i]]]
  for (i in 1:6) dat[[paste0("Q26_", i)]] <- dat[[emo_cols_new[i]]]
  for (i in 1:5) dat[[paste0("Q31_", i)]] <- dat[[ca_cols_new[i]]]
  for (i in 1:4) dat[[paste0("Q13_", i)]] <- dat[[info_avail_new[i]]]
  for (i in 1:4) dat[[paste0("Q12_", i)]] <- dat[[info_cred_new[i]]]
  for (i in 1:3) dat[[paste0("Q15_", i)]] <- dat[[crisis_new[i]]]
  # No Q14 in new export -> misinfo_exposure will be NA
  misinfo_cols <- character(0)
  media_cols <- c(tv_new[tv_new %in% names(dat)], news_new[news_new %in% names(dat)], wa_new[wa_new %in% names(dat)], soc_new[soc_new %in% names(dat)])
} else {
  # Old format: text recodes on Q*_ columns
  cog_cols <- paste0("Q24_", 1:5)
  emo_cols <- paste0("Q26_", 1:6)
  ca_cols <- paste0("Q31_", 1:5)
  info_avail_cols <- paste0("Q13_", 1:4)
  misinfo_cols <- paste0("Q14_", 1:4)
  crisis_norm_cols <- paste0("Q15_", 1:3)
  tv_cols <- paste0("Q9_", 1:9)
  press_cols <- paste0("Q10_", 1:8)
  whatsapp_cols <- "Q11_1"
  trust_cols <- paste0("Q12_", 1:4)
  media_cols <- c(tv_cols, press_cols, whatsapp_cols)
  info_cred_cols <- trust_cols
  for (col in c(cog_cols, ca_cols, info_avail_cols, misinfo_cols, crisis_norm_cols, trust_cols)) {
    if (col %in% names(dat)) dat[[col]] <- recode_agree4(dat[[col]])
  }
  for (col in emo_cols) if (col %in% names(dat)) dat[[col]] <- recode_freq4(dat[[col]])
  for (col in ca_cols) if (col %in% names(dat)) dat[[col]] <- recode_likely4(dat[[col]])
  for (col in media_cols) if (col %in% names(dat)) dat[[col]] <- recode_media_freq5(dat[[col]])
}

item_cols <- c(cog_cols, emo_cols)

# Missingness (LPH items)
miss_cog <- colSums(is.na(dat[, cog_cols[cog_cols %in% names(dat)]]))
miss_emo <- colSums(is.na(dat[, emo_cols[emo_cols %in% names(dat)]]))
msg <- c(msg, "", "---------- Missingness on helplessness items (before listwise) ----------")
msg <- c(msg, paste("Cognitive:", paste(names(miss_cog), "=", miss_cog, collapse = ", ")))
msg <- c(msg, paste("Emotional:", paste(names(miss_emo), "=", miss_emo, collapse = ", ")))

# Listwise on 12 LPH items
dat_main <- dat %>% filter(complete.cases(across(all_of(item_cols))))
n_listwise <- nrow(dat_main)
msg <- c(msg, "", paste("N after listwise deletion on 12 helplessness items:", n_listwise))

# Reverse emotional so higher = more helplessness (except Q26_5 = hopeful)
emo_to_reverse <- setdiff(emo_cols, "Q26_5")
for (col in emo_to_reverse) {
  if (col %in% names(dat_main)) dat_main[[col]] <- 5 - dat_main[[col]]
}

# Composites
dat_main <- dat_main %>%
  mutate(
    info_availability = if (all(info_avail_cols %in% names(.))) rowMeans(across(all_of(info_avail_cols)), na.rm = TRUE) else NA_real_,
    crisis_normalizing = if (all(crisis_norm_cols %in% names(.))) rowMeans(across(all_of(crisis_norm_cols)), na.rm = TRUE) else NA_real_,
    collective_action_intent = if (all(ca_cols %in% names(.))) rowMeans(across(all_of(ca_cols)), na.rm = TRUE) else NA_real_
  )
if (length(media_cols) > 0 && all(media_cols %in% names(dat_main))) {
  dat_main$media_consumption <- rowMeans(dat_main[, media_cols], na.rm = TRUE)
} else {
  dat_main$media_consumption <- rep(NA_real_, nrow(dat_main))
}
if (length(info_cred_cols) > 0 && all(info_cred_cols %in% names(dat_main))) {
  dat_main$info_credibility <- rowMeans(dat_main[, info_cred_cols], na.rm = TRUE)
} else {
  dat_main$info_credibility <- rep(NA_real_, nrow(dat_main))
}
if (length(misinfo_cols) > 0 && all(misinfo_cols %in% names(dat_main))) {
  dat_main$misinfo_exposure <- rowMeans(dat_main[, misinfo_cols], na.rm = TRUE)
} else {
  dat_main$misinfo_exposure <- rep(NA_real_, nrow(dat_main))
}

# Subscale scores
dat_main <- dat_main %>%
  mutate(
    cognitive_score = rowMeans(across(all_of(cog_cols))),
    emotional_score = rowMeans(across(all_of(emo_cols))),
    z_cog = as.numeric(scale(cognitive_score)),
    z_emo = as.numeric(scale(emotional_score)),
    combined_helplessness = (z_cog + z_emo) / 2
  )

# Uprising (new format: perso_2019 1=yes, 2=no; old: Q34 text)
if (new_format && "perso_2019" %in% names(dat_main)) {
  dat_main <- dat_main %>%
    mutate(
      uprising_participated_binary = case_when(
        perso_2019 == "1" | perso_2019 == 1 ~ 1,
        perso_2019 == "2" | perso_2019 == 2 ~ 0,
        TRUE ~ NA_real_
      )
    )
} else if ("Q34" %in% names(dat_main)) {
  dat_main <- dat_main %>%
    mutate(
      uprising_participated_binary = case_when(
        Q34 %in% c("Yes, I participated in person in Lebanon", "Yes, I participated through online engagement", "Yes, I participated in person abroad") ~ 1,
        Q34 == "No, I did not participate" ~ 0,
        TRUE ~ NA_real_
      )
    )
}

# Vignette (if columns exist - old format only)
vignette_core_cols <- c("Q43_1", "Q44_1", "Q52_1", "Q52_2", "Q59_1", "Q59_2", "Q50_1", "Q57_1", "Q62_1", "Q48_1", "Q48_2", "Q48_3", "Q48_4", "Q56_1", "Q56_2", "Q56_3", "Q56_4", "Q63_1", "Q63_2", "Q63_3", "Q63_4")
if (all(vignette_core_cols %in% names(dat_main)) && !new_format) {
  recode_likely4_v <- function(x) { x <- trimws(as.character(x)); out <- rep(NA_real_, length(x)); out[tolower(x) %in% c("extremely unlikely")] <- 1; out[tolower(x) %in% c("somewhat unlikely")] <- 2; out[tolower(x) %in% c("somewhat likely")] <- 3; out[tolower(x) %in% c("extremely likely")] <- 4; out }
  recode_agree4_v <- function(x) { x <- trimws(as.character(x)); out <- rep(NA_real_, length(x)); out[tolower(x) %in% c("strongly disagree")] <- 1; out[tolower(x) %in% c("somewhat disagree")] <- 2; out[tolower(x) %in% c("somewhat agree")] <- 3; out[tolower(x) %in% c("strongly agree")] <- 4; out }
  dat_main <- dat_main %>%
    mutate(
      vignette_condition = case_when(!is.na(Q43_1) ~ "HighInfo", !is.na(Q52_1) ~ "LowInfo", !is.na(Q59_1) ~ "Neutral", TRUE ~ NA_character_),
      v_understanding = as.numeric(coalesce(Q43_1, Q52_1, Q59_1)),
      v_likely_change = as.numeric(coalesce(Q44_1, Q52_2, Q59_2)),
      v_protest_intent = recode_likely4_v(coalesce(Q50_1, Q57_1, Q62_1)),
      v_hope = recode_agree4_v(coalesce(Q48_1, Q56_1, Q63_1)),
      v_anger = recode_agree4_v(coalesce(Q48_2, Q56_2, Q63_2)),
      v_tired = recode_agree4_v(coalesce(Q48_3, Q56_3, Q63_3)),
      v_confused = recode_agree4_v(coalesce(Q48_4, Q56_4, Q63_4))
    )
}

# Descriptives
msg <- c(msg, "", "---------- Score descriptives (analysis sample) ----------")
msg <- c(msg, paste("cognitive_score: mean =", round(mean(dat_main$cognitive_score, na.rm = TRUE), 4), ", SD =", round(sd(dat_main$cognitive_score, na.rm = TRUE), 4)))
msg <- c(msg, paste("emotional_score:  mean =", round(mean(dat_main$emotional_score, na.rm = TRUE), 4), ", SD =", round(sd(dat_main$emotional_score, na.rm = TRUE), 4)))
r_cog_emo <- cor(dat_main$cognitive_score, dat_main$emotional_score, use = "pairwise.complete.obs")
msg <- c(msg, paste("Correlation (cognitive_score, emotional_score): r =", round(r_cog_emo, 4)))
msg <- c(msg, "", "---------- Floor/ceiling ----------")
msg <- c(msg, paste("cognitive: % at min =", round(100 * mean(dat_main$cognitive_score <= 1.01, na.rm = TRUE), 1), ", % at max =", round(100 * mean(dat_main$cognitive_score >= 3.99, na.rm = TRUE), 1)))
msg <- c(msg, paste("emotional:  % at min =", round(100 * mean(dat_main$emotional_score <= 1.01, na.rm = TRUE), 1), ", % at max =", round(100 * mean(dat_main$emotional_score >= 3.99, na.rm = TRUE), 1)))
msg <- c(msg, "", "========== End 01_data_quality_report ==========")

write.csv(dat_main, file.path(output_dir, "learned_helplessness_cleaned.csv"), row.names = FALSE)
writeLines(msg, file.path(output_dir, "01_data_quality_report.txt"))
message("\nSaved: ", file.path(output_dir, "learned_helplessness_cleaned.csv"))
message("Saved: ", file.path(output_dir, "01_data_quality_report.txt"))
