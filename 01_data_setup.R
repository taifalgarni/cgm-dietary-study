# Script 1 - data setup & CGM metrics


library(tidyverse)
library(lubridate)
library(readxl)
library(writexl)

#load the row data - not shared

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
    day = as.integer(day)
  ) %>%
  filter(!is.na(glucose), !is.na(datetime))

# P2 lost CGM on LC, P11 lost CGM on MED (both still completed all three diets)


# 2. compliance / meal data 

meals_raw <- read_excel("Participants complaince .xlsx", sheet = "Sheet1")

meals <- meals_raw %>%
  rename(
    participant       = `Participant ID`,
    day               = Day,
    sex               = Sex,
    meal              = Meal,
    diet              = Diet,
    recipe            = Recipe,
    pct_consumed      = `% consumed`,
    extra             = Extra,
    physical_activity = `Physical activity`,
    kcal_meal         = `Kcal/meal`
  ) %>%
  mutate(
    participant  = factor(str_trim(as.character(participant)), levels = ppt_levels),
    diet         = factor(diet, levels = c("AUS", "MED", "LC")),
    meal         = str_replace(meal, "Breakfaast", "Breakfast"),   # typo in the sheet
    meal         = factor(meal, levels = c("Breakfast", "Lunch", "Dinner")),
    pct_consumed = as.numeric(pct_consumed),
    kcal_meal    = as.numeric(kcal_meal),
    had_extra    = !is.na(extra) & str_trim(extra) != "NA")


# 3. biometrics (one row per person) 

biometrics <- cgm %>%
  select(participant, Sex, AgeGrp, Pre.weight, Height,
         PreS.BMI, PostS.BMI, Visceral.Adiposity,
         Lipids, IGT, Family, Smoking, Activity,
         PCOS, Thyroid, Diet.Routine, Ethnicity) %>%
  distinct(participant, .keep_all = TRUE) %>%
  mutate(across(c(PreS.BMI, PostS.BMI, AgeGrp, Pre.weight, Height), as.numeric))
# P15 has no pre-study weight


# 4. helper: standard CGM metrics for a glucose vector 
# reference thresholds (mmol/L): TIR 3.9-10.0, TITR 3.9-7.8, TAR >10.0, TBR <3.9

cgm_summary <- function(g) {
  g <- g[!is.na(g)]
  tibble(
    n_readings   = length(g),
    mean_glucose = mean(g),
    sd_glucose   = sd(g),
    cv_glucose   = sd(g) / mean(g) * 100,
    TIR  = mean(g >= 3.9 & g <= 10.0) * 100,
    TITR = mean(g >= 3.9 & g <= 7.8)  * 100,
    TAR  = mean(g > 10.0) * 100,
    TBR  = mean(g < 3.9)  * 100)}

# MAGE for one day's (time-ordered) glucose vector.
# Service definition: mean amplitude of excursions bigger than 1 SD of that day.
# done PER DAY and averaged later, so gaps between days can't invent fake swings

mage_day <- function(g) {
  g <- g[!is.na(g)]
  if (length(g) < 3) return(NA_real_)
  thr <- sd(g)
  if (is.na(thr) || thr == 0) return(0)
  # collapse consecutive duplicate readings (plateaus)
  gc <- g[c(TRUE, g[-1] != g[-length(g)])]
  m  <- length(gc)
  if (m < 3) return(0)
  # mark interior turning points (higher or lower than both neighbours)
  is_tp <- logical(m)
  for (i in 2:(m - 1)) {
    if ((gc[i] > gc[i-1] && gc[i] > gc[i+1]) ||
        (gc[i] < gc[i-1] && gc[i] < gc[i+1])) is_tp[i] <- TRUE
  }
  tp     <- gc[c(TRUE, is_tp[2:(m - 1)], TRUE)]   # turning points + the two endpoints
  swings <- abs(diff(tp))
  big    <- swings[swings > thr]
  if (length(big) == 0) return(0)
  mean(big)}


# 5. per-diet CGM metrics 

# overall (whole-period) metrics per participant x diet
metrics_overall <- cgm %>%
  group_by(participant, diet) %>%
  group_modify(~ cgm_summary(.x$glucose)) %>%
  ungroup()

# MAGE - per day first, then averaged per participant x diet
mage_tbl_daily <- cgm %>%
  arrange(participant, diet, day, datetime) %>%
  group_by(participant, diet, day) %>%
  summarise(MAGE = mage_day(glucose), .groups = "drop")

mage_tbl <- mage_tbl_daily %>%
  group_by(participant, diet) %>%
  summarise(MAGE = mean(MAGE, na.rm = TRUE), .groups = "drop")

cgm_metrics <- metrics_overall %>%
  left_join(daytime_mean, by = c("participant", "diet")) %>%
  left_join(mage_tbl,     by = c("participant", "diet"))


# 6. per-DAY metrics (within-diet replication for the aim-3 models) 

daily_metrics <- cgm %>%
  group_by(participant, diet, day) %>%
  group_modify(~ cgm_summary(.x$glucose)) %>%
  ungroup() %>%
  left_join(
    cgm %>% mutate(hr = hour(datetime)) %>%
      filter(hr >= 6, hr < 22) %>%
      group_by(participant, diet, day) %>%
      summarise(daytime_glucose = mean(glucose, na.rm = TRUE), .groups = "drop"),
    by = c("participant", "diet", "day")
  ) %>%
  left_join(mage_tbl_daily, by = c("participant", "diet", "day"))


# 7. feasibility / adherence (Aim 1) 

compliance <- meals %>%
  group_by(participant, diet) %>%
  summarise(
    n_meals           = n(),
    mean_pct_consumed = mean(pct_consumed, na.rm = TRUE),
    n_full_meals      = sum(pct_consumed == 100, na.rm = TRUE),
    n_missed          = sum(pct_consumed == 0,   na.rm = TRUE),
    n_partial         = sum(pct_consumed > 0 & pct_consumed < 100, na.rm = TRUE),
    pct_full          = n_full_meals / n_meals * 100,
    kcal_consumed     = sum(kcal_meal * pct_consumed / 100, na.rm = TRUE),
    kcal_prescribed   = sum(kcal_meal, na.rm = TRUE),
    kcal_compliance   = kcal_consumed / kcal_prescribed * 100,
    n_extras          = sum(had_extra, na.rm = TRUE),
    .groups = "drop")


# 8. diet order / period from the randomisation code 
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
      sequence    = seq_label))}}

diet_order$diet   <- factor(diet_order$diet, levels = c("AUS", "MED", "LC"))
diet_order$period <- factor(diet_order$period)

# sanity check vs the meal diary order
meal_first_day <- meals %>% group_by(participant, diet) %>%
  summarise(first_day = min(day), .groups = "drop")
chk <- merge(diet_order, meal_first_day, by = c("participant", "diet"))
mismatch <- c()
for (p in unique(chk$participant)) {
  s <- chk[chk$participant == p, ]; s <- s[order(s$first_day), ]
  if (!all(s$period == 1:nrow(s))) mismatch <- c(mismatch, as.character(p))
}
if (length(mismatch))
  warning("diet order disagrees with the meal diary for: ", paste(mismatch, collapse = ", "))


# 9. assemble the two analysis datasets 

# per participant x diet  (main comparison + descriptives)
full_data <- cgm_metrics %>%
  left_join(compliance, by = c("participant", "diet")) %>%
  left_join(diet_order, by = c("participant", "diet")) %>%
  left_join(biometrics, by = "participant") %>%
  mutate(
    diet      = relevel(factor(diet), ref = "AUS"),   # AUS is just the model reference
    age_group = factor(ifelse(AgeGrp   < 50, "Under 50", "50 and over")),
    bmi_group = factor(ifelse(PreS.BMI < 25, "Normal",   "Overweight")) )

# per participant x diet x day  (for the person x diet / reliability work)
daily_data <- daily_metrics %>%
  left_join(diet_order %>% distinct(participant, diet, period, sequence),
            by = c("participant", "diet")) %>%
  left_join(biometrics, by = "participant") %>%
  mutate(diet = relevel(factor(diet), ref = "AUS"))


# 10. quick distribution check 
# just to see which outcomes are skewed - script 2 reports both parametric and
# non-parametric anyway, so this is only for our eyes.

check_outcomes <- c("mean_glucose", "daytime_glucose", "sd_glucose", "cv_glucose",
                    "MAGE", "TIR", "TITR", "TAR", "TBR")

normality <- data.frame()
for (out in check_outcomes) {
  for (d in c("AUS", "MED", "LC")) {
    v <- full_data[[out]][full_data$diet == d]; v <- v[!is.na(v)]
    if (length(v) < 3 || length(unique(v)) < 2) {
      normality <- rbind(normality, data.frame(Outcome = out, Diet = d,
                                               N = length(v), p = NA, normal = "-")); next }
    p <- shapiro.test(v)$p.value
    normality <- rbind(normality, data.frame(Outcome = out, Diet = d, N = length(v),
                                             p = round(p, 4), normal = ifelse(p > .05, "yes", "no")))}}


# 11. save 

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
