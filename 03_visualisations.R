# ============================================================
# Script 3 - Figures used in the thesis
# CGM dietary study: AUS vs MED vs LC
#
# Figure 1 - Meal compliance
# Figure 2 - CGM outcomes panel (mean glucose, CV%, MAGE, TIR, TITR, TBR)
# Figure 3 - Ambulatory glucose profile (diurnal curves)
# Figure 4 - Age vs Time in Range, by diet and sex
# ============================================================

library(tidyverse)
library(patchwork)

load("CGM_Study.RData")

diet_cols <- c("AUS" = "#4C9BE8", "MED" = "#E88B4C", "LC" = "#4CE897")

theme_cgm <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
    plot.subtitle    = element_text(hjust = 0.5, colour = "grey40", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )


# Figure 1 - meal compliance ---------------------------------

p_compliance <- ggplot(full_data,
                       aes(x = diet, y = mean_pct_consumed, fill = diet, colour = diet)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.85) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
  scale_fill_manual(values   = diet_cols) +
  scale_colour_manual(values = diet_cols) +
  scale_y_continuous(limits = c(70, 105)) +
  labs(x = "Diet", y = "Mean % of Prescribed Meal Consumed",
       title = "Meal Compliance by Dietary Intervention") +
  theme_cgm +
  theme(legend.position = "none")

ggsave("figure_1_compliance.png", p_compliance, width = 8, height = 6, dpi = 300, bg = "white")


# Figure 2 - CGM outcomes panel ------------------------------
# Six reported outcomes. TAR is omitted (negligible on all diets,
# reported in the table instead).

make_boxplot <- function(data, y_var, y_label, title, ref_line = NULL) {
  p <- ggplot(data, aes(x = diet, y = .data[[y_var]], fill = diet)) +
    geom_violin(alpha = 0.4, width = 0.8) +
    geom_boxplot(width = 0.2, alpha = 0.85, outlier.shape = NA) +
    geom_jitter(aes(colour = diet), width = 0.08, size = 3, alpha = 0.85) +
    scale_fill_manual(values   = diet_cols) +
    scale_colour_manual(values = diet_cols) +
    labs(title = title, x = "Diet", y = y_label) +
    theme_cgm +
    theme(legend.position = "none")
  if (!is.null(ref_line))
    p <- p + geom_hline(yintercept = ref_line, linetype = "dashed",
                        colour = "grey40", linewidth = 0.8)
  p
}

p_mean <- make_boxplot(full_data, "mean_glucose", "Mean Glucose (mmol/L)", "Mean Glucose")
p_cv   <- make_boxplot(full_data, "cv_glucose", "CV (%)", "Glucose Variability (CV%)")
p_mage <- make_boxplot(full_data, "MAGE", "MAGE (mmol/L)", "Mean Amplitude of Glycaemic Excursions")
p_tir  <- make_boxplot(full_data, "TIR", "TIR (%)", "Time in Range (3.9-10.0 mmol/L)", ref_line = 70)
p_titr <- make_boxplot(full_data, "TITR", "TITR (%)", "Time in Tight Range (3.9-7.8 mmol/L)", ref_line = 70)
p_tbr  <- make_boxplot(full_data, "TBR", "TBR (%)", "Time Below Range (<3.9 mmol/L)", ref_line = 4)

p_panel <- (p_mean | p_cv | p_mage) /
           (p_tir | p_titr | p_tbr) +
  plot_annotation(
    title = "CGM Outcomes by Dietary Intervention",
    theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15))
  )

ggsave("figure_2_cgm_outcomes.png", p_panel, width = 15, height = 10, dpi = 300, bg = "white")


# Figure 3 - ambulatory glucose profile (diurnal curves) -----
# Each participant's median glucose per 30-min slot, then
# percentiles across participants per diet.

individual_medians <- cgm %>%
  mutate(
    time_min   = as.numeric(format(datetime, "%H")) * 60 +
                 as.numeric(format(datetime, "%M")),
    time_30min = floor(time_min / 30) * 30
  ) %>%
  group_by(participant, diet, time_30min) %>%
  summarise(glucose_median = median(glucose, na.rm = TRUE), .groups = "drop")

agp_daily <- individual_medians %>%
  group_by(diet, time_30min) %>%
  summarise(
    p05    = quantile(glucose_median, 0.05, na.rm = TRUE),
    p25    = quantile(glucose_median, 0.25, na.rm = TRUE),
    median = median(glucose_median,         na.rm = TRUE),
    p75    = quantile(glucose_median, 0.75, na.rm = TRUE),
    p95    = quantile(glucose_median, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

p_agp <- ggplot(agp_daily, aes(x = time_30min, colour = diet, fill = diet)) +
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
  labs(title = "Ambulatory Glucose Profile (AGP) by Diet",
       x = "Time of Day", y = "Glucose (mmol/L)") +
  theme_cgm +
  theme(legend.position  = "none",
        strip.text       = element_text(face = "bold", size = 13, colour = "white"),
        strip.background = element_rect(fill = "grey25", colour = NA),
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 8))

ggsave("figure_3_agp.png", p_agp, width = 15, height = 6, dpi = 300, bg = "white")


# Figure 4 - Age vs Time in Range, by diet and sex -----------

p_age_tir <- ggplot(full_data, aes(x = AgeGrp, y = TIR, colour = Sex, shape = Sex)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, aes(fill = Sex)) +
  scale_colour_manual(values = c("M" = "#2196F3", "F" = "#E91E90")) +
  scale_fill_manual(values   = c("M" = "#2196F3", "F" = "#E91E90")) +
  facet_wrap(~ diet, ncol = 3) +
  labs(title = "Age vs Time in Range (TIR) by Diet",
       x = "Age (years)", y = "TIR (%)") +
  theme_cgm +
  theme(strip.text = element_text(face = "bold"))

ggsave("figure_4_age_vs_tir.png", p_age_tir, width = 13, height = 5, dpi = 300, bg = "white")
