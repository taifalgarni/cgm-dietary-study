# ============================================================
# Script 4 - Individual response analysis (Aim 3)
# CGM dietary study: AUS vs MED vs LC
#
# 1. Classify each participant's response to each diet relative
#    to their own three-diet mean (mean glucose, CV%, MAGE).
# 2. Rank the diets within each participant into a composite
#    score to find their best diet.
# 3. Test whether the distribution of best diets exceeds chance.
#
# Produces the responder figures, the best-diet figure, and
# CGM_Individual_Response.xlsx.
# ============================================================

library(tidyverse)
library(writexl)

load("CGM_Study.RData")

resp_cols <- c(
  "Strong Responder" = "#1a7a4a",
  "Mild Responder"   = "#4CE897",
  "Non-Responder"    = "#BDC3C7",
  "Mild Opposite"    = "#E88B4C",
  "Strong Opposite"  = "#E63946"
)

# order participants P1..P23 (not alphabetical P1, P10, P11, ...)
order_participants <- function(p) {
  p   <- as.character(p)
  num <- suppressWarnings(as.numeric(gsub("\\D", "", p)))
  factor(p, levels = unique(p[order(num)]))
}


# 1. Responder classification --------------------------------
# Each value is compared to the participant's own three-diet mean.
# Thresholds are study-defined (a priori), not published cut-offs:
#   mean glucose : % change   (+/-5% mild, +/-10% strong)
#   CV%          : %-pt change (+/-1.5 mild, +/-3.5 strong)
#   MAGE         : mmol/L      (+/-0.2 mild, +/-0.5 strong)
# Lower = more favourable.

classify_glucose <- function(pct) {
  case_when(
    pct <= -10          ~ "Strong Responder",
    pct <= -5           ~ "Mild Responder",
    pct > -5 & pct < 5  ~ "Non-Responder",
    pct >= 5 & pct < 10 ~ "Mild Opposite",
    pct >= 10           ~ "Strong Opposite",
    TRUE                ~ "Non-Responder"
  )
}
classify_cv <- function(d) {
  case_when(
    d <= -3.5           ~ "Strong Responder",
    d <= -1.5           ~ "Mild Responder",
    d > -1.5 & d < 1.5  ~ "Non-Responder",
    d >= 1.5 & d < 3.5  ~ "Mild Opposite",
    d >= 3.5            ~ "Strong Opposite",
    TRUE                ~ "Non-Responder"
  )
}
classify_mage <- function(d) {
  case_when(
    d <= -0.5           ~ "Strong Responder",
    d <= -0.2           ~ "Mild Responder",
    d > -0.2 & d < 0.2  ~ "Non-Responder",
    d >= 0.2 & d < 0.5  ~ "Mild Opposite",
    d >= 0.5            ~ "Strong Opposite",
    TRUE                ~ "Non-Responder"
  )
}

response_data <- full_data %>%
  select(participant, diet, mean_glucose, cv_glucose, MAGE) %>%
  group_by(participant) %>%
  mutate(
    pct_change_glucose = ((mean_glucose - mean(mean_glucose, na.rm = TRUE)) /
                            mean(mean_glucose, na.rm = TRUE)) * 100,
    diff_cv_pts    = cv_glucose - mean(cv_glucose, na.rm = TRUE),
    diff_mage_mmol = MAGE       - mean(MAGE,       na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    glucose_category = factor(classify_glucose(pct_change_glucose), levels = names(resp_cols)),
    cv_category      = factor(classify_cv(diff_cv_pts),             levels = names(resp_cols)),
    mage_category    = factor(classify_mage(diff_mage_mmol),        levels = names(resp_cols)),
    lab_glucose = paste0(round(pct_change_glucose, 1), "%"),
    lab_cv      = paste0(round(diff_cv_pts, 1), " pts"),
    lab_mage    = paste0(round(diff_mage_mmol, 3), " mmol/L")
  )


# 2. Responder heatmaps (one per metric) ---------------------

make_heatmap <- function(df, fill_col, label_col, title) {
  df$participant <- order_participants(df$participant)
  df$txt <- ifelse(df[[fill_col]] %in% c("Strong Responder", "Strong Opposite"),
                   "white", "grey20")
  ggplot(df, aes(x = diet, y = participant)) +
    geom_tile(aes(fill = .data[[fill_col]]), colour = "white", linewidth = 0.8) +
    geom_text(aes(label = .data[[label_col]], colour = txt), size = 3.2, fontface = "bold") +
    scale_fill_manual(values = resp_cols) +
    scale_colour_identity() +
    scale_y_discrete(limits = rev) +
    labs(title = title, x = "Diet", y = "Participant", fill = "Response Category") +
    theme_minimal(base_size = 12) +
    theme(plot.title      = element_text(face = "bold", hjust = 0.5, size = 13),
          axis.text.x     = element_text(face = "bold", size = 13),
          legend.position = "bottom",
          panel.grid      = element_blank())
}

ggsave("figure_5a_heatmap_glucose.png",
       make_heatmap(response_data, "glucose_category", "lab_glucose",
                    "Individual Mean Glucose Response per Diet"),
       width = 9, height = 11, dpi = 300, bg = "white")
ggsave("figure_5b_heatmap_cv.png",
       make_heatmap(response_data, "cv_category", "lab_cv",
                    "Individual CV% Response per Diet"),
       width = 9, height = 11, dpi = 300, bg = "white")
ggsave("figure_5c_heatmap_mage.png",
       make_heatmap(response_data, "mage_category", "lab_mage",
                    "Individual MAGE Response per Diet"),
       width = 9, height = 11, dpi = 300, bg = "white")


# 3. Responder summary bar (all three metrics) ---------------

bar_data <- bind_rows(
  response_data %>% group_by(diet) %>% count(glucose_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "Mean Glucose") %>%
    rename(category = glucose_category) %>% ungroup(),
  response_data %>% group_by(diet) %>% count(cv_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "CV%") %>%
    rename(category = cv_category) %>% ungroup(),
  response_data %>% group_by(diet) %>% count(mage_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "MAGE") %>%
    rename(category = mage_category) %>% ungroup()
) %>%
  mutate(metric   = factor(metric, levels = c("Mean Glucose", "CV%", "MAGE")),
         category = factor(category, levels = names(resp_cols)))

p_bar_resp <- ggplot(bar_data, aes(x = diet, y = pct, fill = category)) +
  geom_col(position = "stack", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = ifelse(pct > 5, paste0(n, "\n(", pct, "%)"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.2, fontface = "bold", colour = "white") +
  scale_fill_manual(values = resp_cols) +
  facet_wrap(~ metric, ncol = 3) +
  labs(title = "Responder Classification by Dietary Intervention",
       x = "Diet", y = "% of Participants", fill = "Response Category") +
  theme_minimal(base_size = 13) +
  theme(plot.title       = element_text(face = "bold", hjust = 0.5, size = 14),
        legend.position  = "bottom",
        strip.text       = element_text(face = "bold", size = 11, colour = "white"),
        strip.background = element_rect(fill = "grey25", colour = NA))

ggsave("figure_6_responder_summary.png", p_bar_resp, width = 16, height = 8, dpi = 300, bg = "white")


# 4. Composite ranking - best diet per participant -----------
# Rank the diets within each participant on each metric
# (1 = best = lowest), average the ranks, lowest = winner.

composite <- response_data %>%
  select(participant, diet, mean_glucose, cv_glucose, MAGE) %>%
  filter(!is.na(MAGE)) %>%
  group_by(participant) %>%
  mutate(
    r_glucose = rank(mean_glucose, ties.method = "average"),
    r_cv      = rank(cv_glucose,   ties.method = "average"),
    r_mage    = rank(MAGE,         ties.method = "average"),
    score     = (r_glucose + r_cv + r_mage) / 3,
    win_glucose = diet[which.min(mean_glucose)],
    win_cv      = diet[which.min(cv_glucose)],
    win_mage    = diet[which.min(MAGE)]
  ) %>%
  ungroup()

margins <- composite %>%
  group_by(participant) %>%
  summarise(
    s1 = sort(score)[1],
    s2 = sort(score)[2],
    winner      = diet[which.min(score)],
    win_glucose = first(win_glucose),
    win_cv      = first(win_cv),
    win_mage    = first(win_mage),
    .groups = "drop"
  ) %>%
  mutate(
    win_margin = as.numeric(s2 - s1),
    agreement  = case_when(
      win_glucose == winner & win_cv == winner & win_mage == winner ~ "All 3 agree",
      (win_glucose == winner & win_cv == winner) |
        (win_glucose == winner & win_mage == winner) |
        (win_cv == winner & win_mage == winner)                     ~ "2 of 3 agree",
      TRUE                                                          ~ "Composite only"
    ),
    confidence = recode(agreement,
                        "All 3 agree" = "High", "2 of 3 agree" = "Moderate",
                        "Composite only" = "Low")
  )

win_counts <- margins %>%
  count(winner) %>%
  mutate(pct = round(n / sum(n) * 100, 1))


# 5. Statistical confirmation --------------------------------

# paired Wilcoxon on composite scores (Bonferroni x3, effect size r = Z/sqrt(N))
pairs_list <- list(c("AUS", "MED"), c("AUS", "LC"), c("MED", "LC"))

wilcox_composite <- data.frame()
for (pr in pairs_list) {
  d1 <- composite %>% filter(diet == pr[1]) %>% arrange(participant)
  d2 <- composite %>% filter(diet == pr[2]) %>% arrange(participant)
  common <- intersect(d1$participant, d2$participant)
  x <- d1 %>% filter(participant %in% common) %>% arrange(participant) %>% pull(score)
  y <- d2 %>% filter(participant %in% common) %>% arrange(participant) %>% pull(score)
  n <- min(length(x), length(y)); x <- x[1:n]; y <- y[1:n]

  w     <- wilcox.test(x, y, paired = TRUE, exact = FALSE)
  p_adj <- min(w$p.value * 3, 1)
  r     <- round(abs(qnorm(w$p.value / 2)) / sqrt(n), 3)

  wilcox_composite <- rbind(wilcox_composite, data.frame(
    Comparison  = paste(pr[1], "vs", pr[2]), N_pairs = n,
    median_diff = round(median(x - y), 3),
    p_value     = round(w$p.value, 4),
    p_adjusted  = round(p_adj, 4),
    effect_r    = r,
    Significant = ifelse(p_adj < 0.05, "Yes", "No")))
}

# chi-square: is each diet equally likely to be best?
observed   <- table(margins$winner)
chi_result <- chisq.test(observed, simulate.p.value = TRUE, B = 10000)

# binomial per diet: does it win more than 1/3 of the time?
binom_results <- data.frame()
for (d in c("LC", "AUS", "MED")) {
  n_wins <- sum(margins$winner == d)
  b <- binom.test(n_wins, nrow(margins), p = 1/3, alternative = "greater")
  binom_results <- rbind(binom_results, data.frame(
    diet = d, wins = n_wins, total = nrow(margins), p = round(b$p.value, 4)))
}


# 6. Export --------------------------------------------------

write_xlsx(
  list(
    Responder_Detail = as.data.frame(
      response_data %>%
        select(participant, diet, mean_glucose, pct_change_glucose, glucose_category,
               cv_glucose, diff_cv_pts, cv_category,
               MAGE, diff_mage_mmol, mage_category)),
    Composite_Rankings = as.data.frame(
      composite %>% select(participant, diet, mean_glucose, cv_glucose, MAGE,
                           r_glucose, r_cv, r_mage, score)),
    Best_Diet_Summary = as.data.frame(
      margins %>% select(participant, winner, s1, s2, win_margin, agreement, confidence)),
    Winner_Counts      = as.data.frame(win_counts),
    Wilcoxon_Composite = as.data.frame(wilcox_composite),
    ChiSquare_BestDiet = data.frame(
      Test = c("Chi-square", "Binomial LC", "Binomial AUS", "Binomial MED"),
      p_value = c(round(chi_result$p.value, 4), binom_results$p))
  ),
  "CGM_Individual_Response.xlsx"
)
