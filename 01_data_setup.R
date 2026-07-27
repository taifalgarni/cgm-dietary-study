
# Script 1 - Data setup and CGM metrics
# CGM dietary study: AUS vs MED vs LC
#
# Reads the raw CGM and meal data, calculates the glycaemic
# metrics used in the study, and saves two analysis datasets:
#   full_data  - one row per participant per diet
#   daily_data - one row per participant per diet per day


library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)

# Row data files 

ppt_levels <- paste0("P", 1:23)


# 1. CGM data 

cgm_raw <- read_csv("all_CGM_data.csv", show_col_types = FALSE)

cgm <- cgm_raw %>%
  filter(recordtype == 0) %>%
  rename(participant = subjectID) %>%
  mutate(
    participant = factor(str_trim(participant), levels = ppt_levels),
    diet = factor(diet,
                  levels = c("Australian", "Mediterranean", "Low_carb"),
                  labels = c("AUS", "MED", "LC")),
    datetime = parse_date_time(datetime,
                               orders = c("ymd HMS", "ymd HM", "dmy HMS", "dmy HM"),
                               tz = "UTC"),
    day = as.integer(day)) %>% filter(!is.na(glucose), !is.na(datetime))


# 2. Meal / compliance data 

meals_raw <- read_excel("Participants_compliance.xlsx", sheet = "Sheet1")

meals <- meals_raw %>%
  rename(
    participant  = `Participant ID`,
    day          = Day,
    sex          = Sex,
    meal         = Meal,
    diet         = Diet,
    pct_consumed = `% consumed`,
    extra        = Extra,
    kcal_meal    = `Kcal/meal`
  ) %>%
  mutate(
    participant  = factor(str_trim(as.character(participant)), levels = ppt_levels),
    diet         = factor(diet, levels = c("AUS", "MED", "LC")),
    meal         = str_replace(meal, "Breakfaast", "Breakfast"),
    meal         = factor(meal, levels = c("Breakfast", "Lunch", "Dinner")),
    pct_consumed = as.numeric(pct_consumed),
    kcal_meal    = as.numeric(kcal_meal),
    had_extra    = !is.na(extra) & str_trim(extra) != "NA" )


# 3. Biometrics (one row per participant) 

biometrics <- cgm %>%
  select(participant, Sex, AgeGrp, Pre.weight, Height,
         PreS.BMI, PostS.BMI, Diet.Routine) %>%
  distinct(participant, .keep_all = TRUE) %>%
  mutate(across(c(PreS.BMI, PostS.BMI, AgeGrp, Pre.weight, Height), as.numeric))


# 4. Helper functions 

# Standard CGM metrics for a glucose vector.
# Thresholds (mmol/L): TIR 3.9-10.0, TITR 3.9-7.8, TAR >10.0, TBR <3.9
cgm_summary <- function(g) {
  g <- g[!is.na(g)]
  tibble(
    mean_glucose = mean(g),
    cv_glucose   = sd(g) / mean(g) * 100,
    TIR  = mean(g >= 3.9 & g <= 10.0) * 100,
    TITR = mean(g >= 3.9 & g <= 7.8)  * 100,
    TAR  = mean(g > 10.0) * 100,
    TBR  = mean(g < 3.9)  * 100  )}

# MAGE for one day's glucose vector (Service et al., 1970):
# mean amplitude of excursions larger than 1 SD of that day.
# Calculated per day and averaged later, so gaps between days
# cannot create false excursions.
mage_day <- function(g) {
  g <- g[!is.na(g)]
  if (length(g) < 3) return(NA_real_)
  thr <- sd(g)
  if (is.na(thr) || thr == 0) return(0)
  gc <- g[c(TRUE, g[-1] != g[-length(g)])]   # collapse plateaus
  m  <- length(gc)
  if (m < 3) return(0)
  is_tp <- logical(m)
  for (i in 2:(m - 1)) {
    if ((gc[i] > gc[i-1] && gc[i] > gc[i+1]) ||
        (gc[i] < gc[i-1] && gc[i] < gc[i+1])) is_tp[i] <- TRUE
  }
  tp     <- gc[c(TRUE, is_tp[2:(m - 1)], TRUE)]
  swings <- abs(diff(tp))
  big    <- swings[swings > thr]
  if (length(big) == 0) return(0)
  mean(big)}


# 5. Per-diet metrics (one value per participant per diet) 

metrics_overall <- cgm %>%
  group_by(participant, diet) %>%
  group_modify(~ cgm_summary(.x$glucose)) %>%
  ungroup()

mage_tbl <- cgm %>%
  arrange(participant, diet, day, datetime) %>%
  group_by(participant, diet, day) %>%
  summarise(mage_d = mage_day(glucose), .groups = "drop") %>%
  group_by(participant, diet) %>%
  summarise(MAGE = mean(mage_d, na.rm = TRUE), .groups = "drop")

cgm_metrics <- metrics_overall %>%
  left_join(mage_tbl, by = c("participant", "diet"))


# 6. Per-day metrics (for the inter-individual models) 

daily_metrics <- cgm %>%
  group_by(participant, diet, day) %>%
  group_modify(~ cgm_summary(.x$glucose)) %>%
  ungroup()


# 7. Adherence (Aim 1) 

compliance <- meals %>%
  group_by(participant, diet) %>%
  summarise(
    n_meals           = n(),
    mean_pct_consumed = mean(pct_consumed, na.rm = TRUE),
    n_missed          = sum(pct_consumed == 0, na.rm = TRUE),
    n_partial         = sum(pct_consumed > 0 & pct_consumed < 100, na.rm = TRUE),
    pct_full          = sum(pct_consumed == 100, na.rm = TRUE) / n() * 100,
    n_extras          = sum(had_extra, na.rm = TRUE),
    .groups = "drop"  )


# 8. Diet order / period from the randomisation code 
# Diet.Routine e.g. "AML" = AUS first, MED second, LC third

letter_to_diet <- c(A = "AUS", M = "MED", L = "LC")

diet_order <- data.frame()
for (i in 1:nrow(biometrics)) {
  code      <- as.character(biometrics$Diet.Routine[i])
  letters_i <- strsplit(code, "")[[1]]
  seq_label <- paste(letter_to_diet[letters_i], collapse = "-")
  for (pos in seq_along(letters_i)) {
    diet_order <- rbind(diet_order, data.frame(
      participant = biometrics$participant[i],
      diet        = letter_to_diet[[letters_i[pos]]],
      period      = pos,
      sequence    = seq_label )) }}
diet_order$diet   <- factor(diet_order$diet, levels = c("AUS", "MED", "LC"))
diet_order$period <- factor(diet_order$period)


# 9. Assemble the two analysis datasets 

# AUS is set as the model reference (statistical only, not a control)
full_data <- cgm_metrics %>%
  left_join(compliance, by = c("participant", "diet")) %>%
  left_join(diet_order, by = c("participant", "diet")) %>%
  left_join(biometrics, by = "participant") %>%
  mutate(diet = relevel(factor(diet), ref = "AUS"))

daily_data <- daily_metrics %>%
  left_join(diet_order %>% distinct(participant, diet, period, sequence),
            by = c("participant", "diet")) %>%
  left_join(biometrics, by = "participant") %>%
  mutate(diet = relevel(factor(diet), ref = "AUS"))


# 10. Distribution check (Shapiro-Wilk) 

check_outcomes <- c("mean_glucose", "cv_glucose", "MAGE", "TIR", "TITR", "TAR", "TBR")

normality <- data.frame()
for (out in check_outcomes) {
  for (d in c("AUS", "MED", "LC")) {
    v <- full_data[[out]][full_data$diet == d]; v <- v[!is.na(v)]
    if (length(v) < 3 || length(unique(v)) < 2) next
    p <- shapiro.test(v)$p.value
    normality <- rbind(normality, data.frame(
      Outcome = out, Diet = d, N = length(v),
      p = round(p, 4), normal = ifelse(p > .05, "yes", "no")))}}


# 11. Save 

demographics <- biometrics %>%
  summarise(N = n(),
            age_mean = round(mean(AgeGrp,  na.rm = TRUE), 1),
            age_sd   = round(sd(AgeGrp,    na.rm = TRUE), 1),
            bmi_mean = round(mean(PreS.BMI, na.rm = TRUE), 1),
            bmi_sd   = round(sd(PreS.BMI,   na.rm = TRUE), 1),
            n_male   = sum(Sex == "M", na.rm = TRUE),
            n_female = sum(Sex == "F", na.rm = TRUE))

write_xlsx(list(Demographics = as.data.frame(demographics),
                Compliance   = as.data.frame(compliance),
                Normality    = normality),
           "CGM_Descriptives.xlsx")

save(full_data, daily_data, cgm, meals, biometrics,
     compliance, diet_order, file = "CGM_Study.RData")
