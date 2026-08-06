# Script 3 - visualisations


library(tidyverse)
library(patchwork)

load("CGM_Study.RData")

# one colour scheme for all plots
diet_cols <- c("AUS" = "#4C9BE8",
               "MED" = "#E88B4C",
               "LC"  = "#4CE897")

# one theme for all plots
theme_cgm <- theme_minimal(base_size = 16) +
  theme(
    plot.title       = element_blank(),
    plot.subtitle    = element_blank(),
    legend.position  = "bottom",
    legend.text      = element_text(size = 14, face = "bold"),
    legend.title     = element_text(size = 15, face = "bold"),
    axis.title       = element_text(size = 16, face = "bold"),
    axis.text        = element_text(size = 14, face = "bold"),
    strip.text       = element_text(size = 15, face = "bold"),
    panel.grid.minor = element_blank() )


# 3.1 boxplots - CGM outcomes 
# one combined figure: mean glucose, CV%, MAGE, TITR. plain boxplot + jitter

make_boxplot <- function(data, y_var, y_label) {
  ggplot(data, aes(x = diet, y = .data[[y_var]], fill = diet)) +
    geom_violin(alpha = 0.4, width = 0.8, trim = FALSE) +
    geom_boxplot(width = 0.2, alpha = 0.85, outlier.shape = NA) +
    geom_jitter(aes(colour = diet), width = 0.08, size = 3, alpha = 0.85) +
    scale_fill_manual(values   = diet_cols) +
    scale_colour_manual(values = diet_cols) +
    labs(x = "Diet", y = y_label) +
    theme_cgm +
    theme(legend.position = "none")}

p_mean <- make_boxplot(full_data, "mean_glucose", "Mean Glucose (mmol/L)")
p_cv   <- make_boxplot(full_data, "cv_glucose",   "CV (%)")
p_mage <- make_boxplot(full_data, "MAGE",         "MAGE (mmol/L)")
p_titr <- make_boxplot(full_data, "TITR",         "TITR (%)")

# one combined figure, four panels
p_panel <- (p_mean | p_cv) / (p_mage | p_titr)

ggsave("plot_02_cgm_outcomes_panel.png", p_panel, width = 11, height = 9, dpi = 300, bg = "white")



# 3.2 scatter - Age vs TITR 
# Sex may be coded as "M"/"F" or "Male"/"Female" 


age_titr_data <- full_data %>%
  mutate(Sex = case_when(
    toupper(substr(as.character(Sex), 1, 1)) == "M" ~ "Male",
    toupper(substr(as.character(Sex), 1, 1)) == "F" ~ "Female",
    TRUE ~ as.character(Sex)))

p_age_titr <- ggplot(age_titr_data,
                    aes(x = AgeGrp, y = TITR, colour = Sex, shape = Sex)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, aes(fill = Sex)) +
  scale_colour_manual(values = c("Male" = "#2196F3", "Female" = "#E91E90")) +
  scale_fill_manual(values   = c("Male" = "#2196F3", "Female" = "#E91E90")) +
  facet_wrap(~ diet, ncol = 3) +
  labs(
    x        = "Age (years)",
    y        = "TITR (%)"
  ) +
  theme_cgm

ggsave("plot_03_age_vs_titr.png", p_age_titr, width = 13, height = 5, dpi = 300, bg = "white")


# 3.3 meal compliance 

p_compliance <- ggplot(full_data,
                       aes(x = diet, y = mean_pct_consumed, fill = diet, colour = diet)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.85) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  scale_fill_manual(values   = diet_cols) +
  scale_colour_manual(values = diet_cols) +
  scale_y_continuous(limits = c(70, 105)) +
  annotate("text", x = 3.5, y = 101.5, label = "100% (full compliance)",
           hjust = 1, size = 4.2, colour = "grey40") +
  labs(
    x        = "Diet",
    y        = "Mean % of Prescribed Meal Consumed"
  ) +
  theme_cgm +
  theme(legend.position = "none")

ggsave("plot_04_compliance.png", p_compliance, width = 8, height = 6, dpi = 300, bg = "white")


# 3.4 ambulatory glucose profile (AGP) 
# step 1: each participant's median glucose in each 30-min slot.

individual_medians <- cgm %>%
  mutate(
    time_min   = as.numeric(format(datetime, "%H")) * 60 +
                 as.numeric(format(datetime, "%M")),
    time_30min = floor(time_min / 30) * 30
  ) %>%
  group_by(participant, diet, time_30min) %>%
  summarise(glucose_median = median(glucose, na.rm = TRUE), .groups = "drop")

# step 2: percentiles across participants, per diet per slot.

agp_daily <- individual_medians %>%
  group_by(diet, time_30min) %>%
  summarise(
    p05    = quantile(glucose_median, 0.05, na.rm = TRUE),
    p25    = quantile(glucose_median, 0.25, na.rm = TRUE),
    median = median(glucose_median,         na.rm = TRUE),
    p75    = quantile(glucose_median, 0.75, na.rm = TRUE),
    p95    = quantile(glucose_median, 0.95, na.rm = TRUE),
    .groups = "drop")

p_agp_line <- ggplot(agp_daily, aes(x = time_30min, colour = diet, fill = diet)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3.9, ymax = 10.0,
           fill = "#48BB78", alpha = 0.07) +
  geom_ribbon(aes(ymin = p05, ymax = p95), alpha = 0.15, colour = NA) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.30, colour = NA) +
  geom_line(aes(y = median), linewidth = 1.5) +
  geom_hline(yintercept = 10.0, linetype = "dashed", colour = "#F97316", linewidth = 0.6) +
  geom_hline(yintercept = 3.9,  linetype = "dashed", colour = "#FF4444", linewidth = 0.6) +
  scale_colour_manual(values = diet_cols) +
  scale_fill_manual(values   = diet_cols) +
  scale_x_continuous(breaks = seq(0, 1410, 120),
                     labels = sprintf("%02d:00", seq(0, 23.5, 2))) +
  scale_y_continuous(breaks = seq(2, 14, 2),
                     labels = paste0(seq(2, 14, 2), " mmol/L")) +
  facet_wrap(~ diet, ncol = 3) +
  labs(
    x = "Time of Day", y = "Glucose (mmol/L)"
  ) +
  theme_cgm +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold", size = 15, colour = "white"),
    strip.background = element_rect(fill = "grey25", colour = NA),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 12, face = "bold"))

ggsave("plot_06_agp_daily.png", p_agp_line,
       width = 15, height = 6, dpi = 300, bg = "white")
