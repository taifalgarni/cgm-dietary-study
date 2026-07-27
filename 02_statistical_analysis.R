# ============================================================
# Script 2 - Statistical analysis
# CGM dietary study: AUS vs MED vs LC
#
# Aim 1: feasibility / adherence
# Aim 2: acute glycaemic response and variability (diet vs diet)
# Aim 3: is the diet effect personal, and does it track biometrics
#
# Produces CGM_Results.xlsx (used for Table 2 and Appendix B).
# ============================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(writexl)

load("CGM_Study.RData")

full_data$diet  <- relevel(factor(full_data$diet),  ref = "AUS")
daily_data$diet <- relevel(factor(daily_data$diet), ref = "AUS")

# primary continuous outcomes
lmm_outcomes <- c("mean_glucose", "cv_glucose", "MAGE")
# bounded % outcomes (floor/ceiling -> non-parametric)
np_outcomes  <- c("TIR", "TITR", "TAR", "TBR")

val <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else x


# 1. Aim 1 - adherence ---------------------------------------

adh_wide <- full_data %>%
  select(participant, diet, mean_pct_consumed) %>%
  pivot_wider(names_from = diet, values_from = mean_pct_consumed) %>%
  drop_na()

adherence_friedman <- friedman.test(as.matrix(adh_wide[, -1]))


# 2. Aim 2 - descriptive table -------------------------------

desc_tbl <- full_data %>%
  group_by(diet) %>%
  summarise(across(all_of(c(lmm_outcomes, np_outcomes)),
                   ~ sprintf("%.2f ± %.2f", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))),
            .groups = "drop")


# 3. Aim 2 - linear mixed models (continuous outcomes) -------
# outcome ~ diet + period + (1 | participant)

lmm_summary <- data.frame()
pairwise    <- data.frame()

for (out in lmm_outcomes) {
  m    <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")), data = full_data)
  a    <- anova(m)
  perf <- model_performance(m)

  lmm_summary <- rbind(lmm_summary, data.frame(
    outcome  = out,
    F_diet   = round(a["diet", "F value"], 3),
    p_diet   = round(a["diet", "Pr(>F)"], 4),
    p_period = round(a["period", "Pr(>F)"], 4),
    R2_marg  = round(val(perf$R2_marginal), 3),
    R2_cond  = round(val(perf$R2_conditional), 3),
    ICC      = round(val(perf$ICC), 3)
  ))

  pw <- as.data.frame(emmeans(m, pairwise ~ diet, adjust = "bonferroni")$contrasts)
  pw$outcome <- out
  pairwise <- rbind(pairwise, pw[, c("outcome", "contrast", "estimate", "SE", "t.ratio", "p.value")])
}


# 4. Aim 2 - bounded % outcomes (TIR/TITR/TAR/TBR) -----------
# Friedman across the three diets, then paired Wilcoxon per pair (Bonferroni x3)

diet_pairs <- list(c("AUS", "MED"), c("AUS", "LC"), c("MED", "LC"))

friedman_results <- data.frame()
for (out in np_outcomes) {
  w <- full_data %>%
    select(participant, diet, value = all_of(out)) %>%
    pivot_wider(names_from = diet, values_from = value) %>%
    drop_na()
  if (nrow(w) >= 3) {
    ft <- friedman.test(as.matrix(w[, -1]))
    friedman_results <- rbind(friedman_results, data.frame(
      outcome = out, N = nrow(w),
      chisq = round(as.numeric(ft$statistic), 3),
      df    = as.numeric(ft$parameter),
      p     = round(ft$p.value, 4)))
  }
}

np_pairwise <- data.frame()
for (out in np_outcomes) {
  for (pr in diet_pairs) {
    d <- full_data %>%
      filter(diet %in% pr) %>%
      select(participant, diet, value = all_of(out)) %>%
      pivot_wider(names_from = diet, values_from = value) %>%
      drop_na()
    if (nrow(d) < 3) next
    wt <- wilcox.test(d[[pr[1]]], d[[pr[2]]], paired = TRUE, exact = FALSE)
    np_pairwise <- rbind(np_pairwise, data.frame(
      outcome = out, comparison = paste(pr[1], "vs", pr[2]), N = nrow(d),
      median_diff = round(median(d[[pr[1]]] - d[[pr[2]]]), 3),
      p = round(wt$p.value, 4),
      p_bonf = round(min(wt$p.value * 3, 1), 4)))
  }
}


# 5. Aim 3 - is the diet effect personal? (random slopes) ----
# random-slope vs random-intercept model, compared by LRT.
# Singular fits are kept and reported (they are themselves informative).

slope_results <- data.frame()
for (out in c("mean_glucose", "cv_glucose")) {
  m0 <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")),
             data = daily_data, REML = FALSE)
  m1 <- tryCatch(
    lmer(as.formula(paste(out, "~ diet + period + (diet | participant)")),
         data = daily_data, REML = FALSE),
    error = function(e) NULL)

  if (is.null(m1)) next

  sing <- isSingular(m1)
  lrt  <- anova(m0, m1)
  vc   <- as.data.frame(VarCorr(m1))
  sd_med <- vc$sdcor[vc$grp == "participant" & vc$var1 == "dietMED" & is.na(vc$var2)]
  sd_lc  <- vc$sdcor[vc$grp == "participant" & vc$var1 == "dietLC"  & is.na(vc$var2)]

  slope_results <- rbind(slope_results, data.frame(
    outcome      = out,
    singular     = sing,
    LRT_chisq    = round(lrt$Chisq[2], 3),
    df           = lrt$Df[2],
    p_LRT        = round(lrt$`Pr(>Chisq)`[2], 4),
    slope_sd_MED = round(ifelse(length(sd_med) > 0, sd_med, NA), 3),
    slope_sd_LC  = round(ifelse(length(sd_lc)  > 0, sd_lc,  NA), 3)))
}


# 6. Aim 3 - reliability of each person's best diet ----------
# between-diet spread vs within-diet day-to-day SD.
# ratio > 1 = the best diet is distinguishable from daily noise.

reliab <- daily_data %>%
  group_by(participant, diet) %>%
  summarise(diet_mean = mean(mean_glucose, na.rm = TRUE),
            day_sd    = sd(mean_glucose,   na.rm = TRUE), .groups = "drop") %>%
  group_by(participant) %>%
  summarise(between_diet_range = max(diet_mean) - min(diet_mean),
            mean_day_sd        = mean(day_sd, na.rm = TRUE),
            ratio              = between_diet_range / mean_day_sd,
            .groups = "drop")


# 7. Aim 3 - diet x biometric interactions (exploratory) -----

mod_results <- data.frame()
for (out in c("mean_glucose", "cv_glucose")) {
  for (mod in c("PreS.BMI", "AgeGrp")) {
    m    <- lmer(as.formula(paste(out, "~ diet *", mod, "+ period + (1 | participant)")),
                 data = daily_data)
    a    <- anova(m)
    term <- grep(paste0("diet:", mod), rownames(a), value = TRUE)
    if (length(term))
      mod_results <- rbind(mod_results, data.frame(
        outcome = out, moderator = mod,
        F = round(a[term, "F value"], 3), p = round(a[term, "Pr(>F)"], 4)))
  }
}


# 8. Model checks (continuous outcomes) ----------------------

for (out in c("mean_glucose", "cv_glucose")) {
  m <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")), data = full_data)
  print(check_normality(m))
  print(check_heteroscedasticity(m))
}


# 9. Export --------------------------------------------------

write_xlsx(list(Descriptives = as.data.frame(desc_tbl),
                LMM          = lmm_summary,
                Pairwise     = pairwise,
                Friedman     = friedman_results,
                NP_pairwise  = np_pairwise,
                RandomSlopes = slope_results,
                Moderation   = mod_results,
                Reliability  = as.data.frame(reliab)),
           "CGM_Results.xlsx")
