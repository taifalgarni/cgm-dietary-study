# Script 2 - analysis


library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(writexl)

load("CGM_Study.RData")

# to keep AUS as the reference diet everywhere
full_data$diet  <- relevel(factor(full_data$diet),  ref = "AUS")
daily_data$diet <- relevel(factor(daily_data$diet), ref = "AUS")

# shared colours 
diet_cols <- c("AUS" = "#4C9BE8", "MED" = "#E88B4C", "LC" = "#4CE897")

# continuous outcomes that are fine for a linear model
lmm_outcomes <- c("mean_glucose", "daytime_glucose", "sd_glucose", "cv_glucose", "MAGE")
# bounded % outcome that floors/ceilings -> non-parametric
np_outcomes  <- c("TITR")

# small helper so a missing performance metric doesn't break the rbind
val <- function(x) if (is.null(x) || length(x) == 0) NA_real_ else x


# 1. Aim 1 - feasibility / adherence 
  
adherence_by_diet <- full_data %>%
  group_by(diet) %>%
  summarise(
    N              = n(),
    pct_consumed   = sprintf("%.1f ± %.1f", mean(mean_pct_consumed, na.rm = TRUE),
                             sd(mean_pct_consumed,   na.rm = TRUE)),
    kcal_comp      = sprintf("%.1f ± %.1f", mean(kcal_compliance, na.rm = TRUE),
                             sd(kcal_compliance,   na.rm = TRUE)),
    full_meals_pct = sprintf("%.1f", mean(pct_full, na.rm = TRUE)),
    missed_meals   = sum(n_missed, na.rm = TRUE),
    .groups = "drop")

# did adherence differ between diets? paired, non-parametric.
# pivot to one row per person with a column per diet, drop anyone missing a diet.
adh_wide <- full_data %>%
  select(participant, diet, mean_pct_consumed) %>%
  pivot_wider(names_from = diet, values_from = mean_pct_consumed) %>%
  drop_na()

adherence_friedman <- if (nrow(adh_wide) >= 3) {
  friedman.test(as.matrix(adh_wide[, -1]))
} else {
  NULL
}


# 2. Aim 2 - descriptive table 

desc_tbl <- full_data %>%
  group_by(diet) %>%
  summarise(across(all_of(c(lmm_outcomes, np_outcomes)),
                   ~ sprintf("%.2f ± %.2f", mean(.x, na.rm = TRUE), sd(.x, na.rm = TRUE))),
            .groups = "drop")


# 3. Aim 2 - main diet comparison (continuous outcomes) 
# model for each outcome: outcome ~ diet + period + (1 | participant)
# period is in there because it was a crossover with no washout.

lmm_summary <- data.frame()
pairwise    <- data.frame()

for (out in lmm_outcomes) {
  m <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")), data = full_data)
  a    <- anova(m)
  perf <- model_performance(m)

  lmm_summary <- rbind(lmm_summary, data.frame(
    outcome  = out,
    F_diet   = round(a["diet", "F value"], 3),
    p_diet   = round(a["diet", "Pr(>F)"], 4),
    p_period = round(a["period", "Pr(>F)"], 4),
    R2_marg  = round(val(perf$R2_marginal), 3),
    R2_cond  = round(val(perf$R2_conditional), 3),
    ICC      = round(val(perf$ICC), 3)))

  # all three pairwise diet contrasts, Bonferroni adjusted
  pw <- as.data.frame(emmeans(m, pairwise ~ diet, adjust = "bonferroni")$contrasts)
  pw$outcome <- out
  pairwise <- rbind(pairwise, pw[, c("outcome", "contrast", "estimate", "SE", "t.ratio", "p.value")])}


# 4. Aim 2 - bounded % outcome (TITR) 
# TITR floors/ceilings so a normal model is not appropriate. so I use Friedman
# across the three diets, then paired Wilcoxon for each pair.

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
      p     = round(ft$p.value, 4)))}}

np_pairwise <- data.frame()
for (out in np_outcomes) {
  for (pr in diet_pairs) {
    # pivot so the two diets sit in their own columns, then pair by row.
    # drop_na after the pivot keeps the pairing safe (same person on both sides).
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
      p_bonf = round(min(wt$p.value * 3, 1), 4))) }}


# 5. Aim 3 - is the diet effect personal? (random slopes) 
# daily_data has ~3-4 days per diet per person, so we can let the diet effect
# vary by person and test whether that helps. at this n some fits will be
# singular - which is itself part of the answer (can't separate a personal
# effect from day-to-day noise).

slope_results <- data.frame()
for (out in c("mean_glucose", "cv_glucose", "MAGE")) {

  m0 <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")),
             data = daily_data, REML = FALSE)
  m1 <- tryCatch(
    lmer(as.formula(paste(out, "~ diet + period + (diet | participant)")),
         data = daily_data, REML = FALSE),
    error = function(e) NULL)

  if (is.null(m1)) {
    slope_results <- rbind(slope_results, data.frame(
      outcome = out, singular = NA, LRT_chisq = NA, df = NA, p_LRT = NA,
      slope_sd_MED = NA, slope_sd_LC = NA, slope_sd_MEDvLC = NA))
    next}

  sing <- isSingular(m1)
  lrt  <- anova(m0, m1)                 # LRT is still informative even if singular
  vc   <- as.data.frame(VarCorr(m1))    # tidy variance components

  # pull the slope variances for the two non-reference diets (vcov = raw
  # variance/covariance, not sdcor, so we can combine them algebraically)
  var_med <- vc$vcov[vc$grp == "participant" & vc$var1 == "dietMED" & is.na(vc$var2)]
  var_lc  <- vc$vcov[vc$grp == "participant" & vc$var1 == "dietLC"  & is.na(vc$var2)]
  cov_medlc <- vc$vcov[vc$grp == "participant" & !is.na(vc$var2) &
                        ((vc$var1 == "dietMED" & vc$var2 == "dietLC") |
                         (vc$var1 == "dietLC"  & vc$var2 == "dietMED"))]

  sd_med <- if (length(var_med) > 0) sqrt(var_med) else NA_real_
  sd_lc  <- if (length(var_lc)  > 0) sqrt(var_lc)  else NA_real_

  # Var(MED - LC) = Var(MED) + Var(LC) - 2*Cov(MED, LC) - this is the
  # individual variability in how much better/worse LC is than MED for a
  # given person, not just how each compares to the AUS reference.
  sd_med_lc <- if (length(var_med) > 0 & length(var_lc) > 0 & length(cov_medlc) > 0) {
    sqrt(var_med + var_lc - 2 * cov_medlc)
  } else {
    NA_real_ }

  slope_results <- rbind(slope_results, data.frame(
    outcome        = out,
    singular       = sing,
    LRT_chisq      = round(lrt$Chisq[2], 3),
    df             = lrt$Df[2],
    p_LRT          = round(lrt$`Pr(>Chisq)`[2], 4),
    slope_sd_MED   = round(sd_med, 3),
    slope_sd_LC    = round(sd_lc, 3),
    slope_sd_MEDvLC = round(sd_med_lc, 3)))}


# 6. Aim 3 - reliability of an individual's "optimal" diet 
# for each person, compare their between-diet spread (on daily means) to their
# within-diet day-to-day SD. if the spread isn't bigger than the noise, their
# "optimal" diet isn't really distinguishable from chance. this is the number the
# profile plot below leans on for its caveat.

reliab <- daily_data %>%
  group_by(participant, diet) %>%
  summarise(diet_mean = mean(mean_glucose, na.rm = TRUE),
            day_sd    = sd(mean_glucose,   na.rm = TRUE), .groups = "drop") %>%
  group_by(participant) %>%
  summarise(between_diet_range = max(diet_mean) - min(diet_mean),
            mean_day_sd        = mean(day_sd, na.rm = TRUE),
            ratio              = between_diet_range / mean_day_sd,
            .groups = "drop")

n_reliable <- sum(reliab$ratio > 1, na.rm = TRUE)
n_total    <- sum(!is.na(reliab$ratio))


# 7. Aim 3 - does the diet effect depend on biometrics? 
# diet x BMI and diet x age, continuous. exploratory / underpowered for
# interactions - we just want the interaction p and a direction.

mod_results <- data.frame()
for (out in c("mean_glucose", "cv_glucose", "MAGE")) {
  for (mod in c("PreS.BMI", "AgeGrp")) {
    m <- lmer(as.formula(paste(out, "~ diet *", mod, "+ period + (1 | participant)")),
              data = daily_data)
    a    <- anova(m)
    term <- grep(paste0("diet:", mod), rownames(a), value = TRUE)
    if (length(term))
      mod_results <- rbind(mod_results, data.frame(
        outcome = out, moderator = mod,
        F = round(a[term, "F value"], 3), p = round(a[term, "Pr(>F)"], 4))) }}


# 8. Aim 3 - per-person profile plot 
# the "optimal diet per participant" ranking is entirely in Script 4
# (composite of mean glucose, CV% and MAGE), so it is NOT duplicated here.


prof <- full_data %>%
  filter(!is.na(mean_glucose)) %>%
  group_by(participant) %>%
  mutate(person_mean = mean(mean_glucose),
         centred     = mean_glucose - person_mean) %>%   # within-person deviation
  ungroup() %>%
  mutate(diet = factor(as.character(diet), levels = c("AUS", "MED", "LC")))

# group means for the bold overlay line, on each scale
mean_raw <- prof %>% group_by(diet) %>%
  summarise(y = mean(mean_glucose), .groups = "drop")
mean_ctr <- prof %>% group_by(diet) %>%
  summarise(y = mean(centred), .groups = "drop")

plot_theme <- theme_minimal(base_size = 14) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    axis.title       = element_text(size = 14, face = "bold"),
    axis.text        = element_text(size = 13, face = "bold"),
    legend.text      = element_text(size = 13, face = "bold"),
    legend.title     = element_text(size = 14, face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank())

# left panel: raw means
p_raw <- ggplot(prof, aes(diet, mean_glucose, group = participant)) +
  geom_line(colour = "grey75", alpha = 0.7, linewidth = 0.5) +
  geom_point(aes(colour = diet), size = 2, alpha = 0.85) +
  geom_line(data = mean_raw, aes(diet, y, group = 1),
            colour = "grey15", linewidth = 1.6) +
  geom_point(data = mean_raw, aes(diet, y, group = 1),
             colour = "grey15", size = 3.5) +
  scale_colour_manual(values = diet_cols) +
  labs(x = NULL, y = "Mean glucose (mmol/L)", colour = "Diet") +
  plot_theme

# right panel: within-person centred (baseline removed)
p_ctr <- ggplot(prof, aes(diet, centred, group = participant)) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.6) +
  geom_line(colour = "grey75", alpha = 0.7, linewidth = 0.5) +
  geom_point(aes(colour = diet), size = 2, alpha = 0.85) +
  geom_line(data = mean_ctr, aes(diet, y, group = 1),
            colour = "grey15", linewidth = 1.6) +
  geom_point(data = mean_ctr, aes(diet, y, group = 1),
             colour = "grey15", size = 3.5) +
  scale_colour_manual(values = diet_cols) +
  labs(x = NULL, y = "Change from person's own mean (mmol/L)", colour = "Diet") +
  plot_theme

p_profiles <- (p_raw | p_ctr) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("person_diet_profiles.png", p_profiles,
       width = 11, height = 5.5, dpi = 300, bg = "white")


# 9. model checks (continuous outcomes) 

model_checks <- list()
for (out in c("mean_glucose", "cv_glucose", "MAGE")) {
  m <- lmer(as.formula(paste(out, "~ diet + period + (1 | participant)")), data = full_data)
  model_checks[[out]] <- list(
    normality        = check_normality(m),
    heteroscedasticity = check_heteroscedasticity(m) )}


# 10. Save 

write_xlsx(list(Descriptives   = as.data.frame(desc_tbl),
                Adherence      = as.data.frame(adherence_by_diet),
                LMM            = lmm_summary,
                Pairwise       = pairwise,
                Friedman       = friedman_results,
                NP_pairwise    = np_pairwise,
                RandomSlopes   = slope_results,
                Moderation     = mod_results,
                Reliability    = as.data.frame(reliab)),
           "CGM_Results.xlsx")
