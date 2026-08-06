# Script 4 - individual response analysis


library(tidyverse)
library(patchwork)
library(writexl)

load("CGM_Study.RData")

diet_cols <- c("AUS" = "#4C9BE8", "MED" = "#E88B4C", "LC" = "#4CE897")

# response categories and their colours
resp_cols <- c(
  "Strong Responder" = "#1a7a4a",
  "Mild Responder"   = "#4CE897",
  "Non-Responder"    = "#BDC3C7",
  "Mild Opposite"    = "#E88B4C",
  "Strong Opposite"  = "#E63946")

# put participants in P1..P23 order no matter how the column is stored
# ("P1" / "1" / factor). plain alphabetical would give P1, P10, P11, ... P2,
# so pull the number out and sort on that. plots add scale_y_discrete(limits =
# rev) so P1 ends up at the TOP of the axis and P23 at the bottom.
order_participants <- function(p) {
  p   <- as.character(p)
  num <- suppressWarnings(as.numeric(gsub("\\D", "", p)))
  factor(p, levels = unique(p[order(num)]))}


# 4.2 responder classification - 3 metrics 
# everything is measured against each person's own grand mean across the three
# diets, so people with different baseline glucose are treated fairly.
#
#   mean glucose : % change from grand mean       (+/-5% mild, +/-10% strong)
#   CV%          : %-point change from grand mean (+/-1.5 mild, +/-3.5 strong)
#   MAGE         : mmol/L change from grand mean  (+/-0.2 mild, +/-0.5 strong)
#
# thresholds are pre-specified / pragmatic - they sit just under the group-level
# pairwise differences from Script 2 and are NOT published cut-offs, so its reported
# as study-defined.

classify_glucose <- function(pct) {
  case_when(
    pct <= -10          ~ "Strong Responder",
    pct <= -5           ~ "Mild Responder",
    pct > -5 & pct < 5  ~ "Non-Responder",
    pct >= 5 & pct < 10 ~ "Mild Opposite",
    pct >= 10           ~ "Strong Opposite",
    TRUE                ~ "Non-Responder" )}

classify_cv <- function(diff_pts) {
  case_when(
    diff_pts <= -3.5                 ~ "Strong Responder",
    diff_pts <= -1.5                 ~ "Mild Responder",
    diff_pts > -1.5 & diff_pts < 1.5 ~ "Non-Responder",
    diff_pts >= 1.5 & diff_pts < 3.5 ~ "Mild Opposite",
    diff_pts >= 3.5                  ~ "Strong Opposite",
    TRUE                             ~ "Non-Responder")}

classify_mage <- function(diff_mmol) {
  case_when(
    diff_mmol <= -0.5                   ~ "Strong Responder",
    diff_mmol <= -0.2                   ~ "Mild Responder",
    diff_mmol > -0.2 & diff_mmol < 0.2  ~ "Non-Responder",
    diff_mmol >= 0.2 & diff_mmol < 0.5  ~ "Mild Opposite",
    diff_mmol >= 0.5                    ~ "Strong Opposite",
    TRUE                                ~ "Non-Responder")}

response_data <- full_data %>%
  select(participant, diet, mean_glucose, cv_glucose, MAGE) %>%
  group_by(participant) %>%
  mutate(
    grand_mean_glucose = mean(mean_glucose, na.rm = TRUE),
    grand_mean_cv      = mean(cv_glucose,   na.rm = TRUE),
    grand_mean_mage    = mean(MAGE,         na.rm = TRUE),
    pct_change_glucose = ((mean_glucose - grand_mean_glucose) / grand_mean_glucose) * 100,
    diff_cv_pts        = cv_glucose - grand_mean_cv,
    diff_mage_mmol     = MAGE       - grand_mean_mage
  ) %>%
  ungroup() %>%
  mutate(
    glucose_category = factor(classify_glucose(pct_change_glucose), levels = names(resp_cols)),
    cv_category      = factor(classify_cv(diff_cv_pts),             levels = names(resp_cols)),
    mage_category    = factor(classify_mage(diff_mage_mmol),        levels = names(resp_cols))
  ) %>%
  left_join(
    full_data %>%
      select(participant, Sex, AgeGrp, PreS.BMI) %>%
      distinct(participant, .keep_all = TRUE),
    by = "participant" )

# pre-build the text that goes inside each heatmap tile (simpler than doing it
# inside the plotting call)
response_data <- response_data %>%
  mutate(
    lab_glucose = paste0(round(pct_change_glucose, 1), "%"),
    lab_cv      = paste0(round(diff_cv_pts, 1), " pts"),
    lab_mage    = paste0(round(diff_mage_mmol, 3), " mmol/L"))


# 4.3 responder summaries per diet 

responder_summary <- data.frame()
for (metric in c("glucose_category", "cv_category", "mage_category")) {
  lbl <- recode(metric,
                glucose_category = "Mean Glucose (% change)",
                cv_category      = "CV% (absolute %pts)",
                mage_category    = "MAGE (absolute mmol/L)")
  for (d in c("AUS", "MED", "LC")) {
    sub <- response_data %>% filter(diet == d)
    tab <- sub %>%
      count(.data[[metric]]) %>%
      mutate(pct = round(n / sum(n) * 100, 1),
             metric = lbl, diet = d)
    names(tab)[1] <- "category"
    responder_summary <- rbind(responder_summary, tab)}}


# 4.4 responder heatmaps - 3 metrics 
# one tile per participant x diet, coloured by response category, with the
# actual change written in. participants run P1 (top) to P23 (bottom).

make_heatmap <- function(df, fill_col, label_col, subtitle) {
  df$participant <- order_participants(df$participant)
  # white text on the dark "strong" tiles, dark text on the rest
  df$txt <- ifelse(df[[fill_col]] %in% c("Strong Responder", "Strong Opposite"),
                   "white", "grey20")

  ggplot(df, aes(x = diet, y = participant)) +
    geom_tile(aes(fill = .data[[fill_col]]), colour = "white", linewidth = 0.8) +
    geom_text(aes(label = .data[[label_col]], colour = txt),
              size = 4.8, fontface = "bold") +
    scale_fill_manual(values = resp_cols) +
    scale_colour_identity() +
    scale_y_discrete(limits = rev) +   # P1 at top, P23 at bottom
    labs(subtitle = subtitle,
         x = "Diet", y = "Participant", fill = "Response Category") +
    theme_minimal(base_size = 16) +
    theme(
      plot.subtitle   = element_text(hjust = 0.5, colour = "grey30", size = 13, face = "bold"),
      axis.text.x     = element_text(face = "bold", size = 17),
      axis.text.y     = element_text(size = 14, face = "bold"),
      axis.title      = element_text(size = 15, face = "bold"),
      legend.position = "bottom",
      legend.text     = element_text(size = 13, face = "bold"),
      legend.title    = element_text(size = 14, face = "bold"),
      panel.grid      = element_blank())}

p_heat_glucose <- make_heatmap(
  response_data, "glucose_category", "lab_glucose",
  "% change from participant's own grand mean\nThreshold: +/-5% = mild | +/-10% = strong")

p_heat_cv <- make_heatmap(
  response_data, "cv_category", "lab_cv",
  "Absolute %pt change from participant's own grand mean CV\nThreshold: ±1.5 %pts = mild | ±3.5 %pts = strong")

p_heat_mage <- make_heatmap(
  response_data, "mage_category", "lab_mage",
  "Absolute mmol/L change from participant's own grand mean MAGE\nThreshold: ±0.2 mmol/L = mild | ±0.5 mmol/L = strong")

# combined single-image version for the thesis (Figure 6) - avoids manually
# tiling three separate PNGs side by side in Word, which made the text
# unreadably small previously. One shared legend at the bottom, one file.
p_heat_combined <- (p_heat_glucose + theme(legend.position = "none")) +
  (p_heat_cv     + theme(legend.position = "none", axis.title.y = element_blank())) +
  (p_heat_mage   + theme(legend.position = "none", axis.title.y = element_blank())) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave("plot_07_09_heatmap_combined.png", p_heat_combined,
       width = 20, height = 11, dpi = 300, bg = "white")


# 4.5 responder summary bar - all 3 metrics 
# stacked %, one panel per metric, so you can see the category mix per diet.

bar_data <- bind_rows(
  response_data %>% group_by(diet) %>% count(glucose_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "Mean Glucose (±5%|±10%)") %>%
    rename(category = glucose_category) %>% ungroup(),
  response_data %>% group_by(diet) %>% count(cv_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "CV% (±1.5|±3.5 pts)") %>%
    rename(category = cv_category) %>% ungroup(),
  response_data %>% group_by(diet) %>% count(mage_category) %>%
    mutate(pct = round(n / sum(n) * 100, 1), metric = "MAGE (±0.2|±0.5 mmol/L)") %>%
    rename(category = mage_category) %>% ungroup()
) %>%
  mutate(
    metric   = factor(metric, levels = c("Mean Glucose (±5%|±10%)",
                                         "CV% (±1.5|±3.5 pts)",
                                         "MAGE (±0.2|±0.5 mmol/L)")),
    category = factor(category, levels = names(resp_cols)))

p_bar_resp <- ggplot(bar_data, aes(x = diet, y = pct, fill = category)) +
  geom_col(position = "stack", width = 0.6, alpha = 0.9) +
  geom_text(aes(label = ifelse(pct > 5, paste0(n, "\n(", pct, "%)"), "")),
            position = position_stack(vjust = 0.5),
            size = 3.6, fontface = "bold", colour = "white") +
  scale_fill_manual(values = resp_cols) +
  facet_wrap(~ metric, ncol = 3) +
  labs(subtitle = "Mean Glucose: % change | CV%: %pt change | MAGE: mmol/L change",
       x = "Diet", y = "% of Participants", fill = "Response Category") +
  theme_minimal(base_size = 15) +
  theme(
    plot.subtitle       = element_text(hjust = 0.5, colour = "grey30", size = 13, face = "bold"),
    legend.position     = "bottom",
    legend.text         = element_text(size = 13, face = "bold"),
    legend.title        = element_text(size = 14, face = "bold"),
    axis.title          = element_text(size = 15, face = "bold"),
    axis.text           = element_text(size = 13, face = "bold"),
    strip.text          = element_text(face = "bold", size = 13, colour = "white"),
    strip.background    = element_rect(fill = "grey25", colour = NA),
    panel.grid.major.x  = element_blank() )

ggsave("plot_10_responder_summary.png", p_bar_resp, width = 16, height = 8, dpi = 300, bg = "white")


# 4.6 composite ranking - best diet per participant 
# rank the three diets within each person on each metric (1 = best = lowest),
# average the ranks, and the lowest average is that person's winner. also keep
# the single-metric winners so we can see how often the metrics agree.

composite <- response_data %>%
  select(participant, diet, mean_glucose, cv_glucose, MAGE) %>%
  filter(!is.na(MAGE)) %>%
  group_by(participant) %>%
  mutate(
    r_glucose   = rank(mean_glucose, ties.method = "average"),
    r_cv        = rank(cv_glucose,   ties.method = "average"),
    r_mage      = rank(MAGE,         ties.method = "average"),
    score       = (r_glucose + r_cv + r_mage) / 3,
    win_glucose = diet[which.min(mean_glucose)],
    win_cv      = diet[which.min(cv_glucose)],
    win_mage    = diet[which.min(MAGE)]
  ) %>%
  ungroup()

# one row per person: winner (or tie), runner-up gap, and whether the
# metrics agree. ties are detected by comparing each diet's score directly
# once pivoted wide, using a small tolerance for floating-point rounding.

tie_tolerance <- 1e-6

scores_wide <- composite %>%
  select(participant, diet, score, win_glucose, win_cv, win_mage) %>%
  pivot_wider(names_from = diet, values_from = score, names_prefix = "score_")

margins <- scores_wide %>%
  rowwise() %>%
  mutate(
    min_score  = min(c(score_AUS, score_MED, score_LC), na.rm = TRUE),
    s2         = sort(c(score_AUS, score_MED, score_LC))[2],
    tied_AUS   = !is.na(score_AUS) & abs(score_AUS - min_score) <= tie_tolerance,
    tied_MED   = !is.na(score_MED) & abs(score_MED - min_score) <= tie_tolerance,
    tied_LC    = !is.na(score_LC)  & abs(score_LC  - min_score) <= tie_tolerance,
    n_tied_at_min = sum(tied_AUS, tied_MED, tied_LC),
    is_tied    = n_tied_at_min > 1,
    winner     = if (is_tied) {
      paste("Tied:", paste(c("AUS","MED","LC")[c(tied_AUS, tied_MED, tied_LC)], collapse = "/"))
    } else {
      c("AUS","MED","LC")[c(tied_AUS, tied_MED, tied_LC)][1]
    },
    win_margin = as.numeric(s2 - min_score),
    clarity    = case_when(
      is_tied           ~ "Tied",
      win_margin >= 1.0 ~ "Clear Winner",
      win_margin >= 0.5 ~ "Moderate",
      TRUE              ~ "Close Call"
    ),
    agreement  = if (is_tied) {
      NA_character_
    } else if (win_glucose == winner & win_cv == winner & win_mage == winner) {
      "All 3 agree"
    } else if ((win_glucose == winner & win_cv == winner) |
               (win_glucose == winner & win_mage == winner) |
               (win_cv == winner & win_mage == winner)) {
      "2 of 3 agree"
    } else {
      "Composite only"
    }
  ) %>%
  ungroup() %>%
  select(participant, winner, win_margin, clarity, agreement, is_tied,
         win_glucose, win_cv, win_mage)

# winner counts (used by the chi-square test and the Excel export) - ties get
# their own category rather than being forced into one diet or the other
win_counts <- margins %>%
  count(winner) %>%
  mutate(pct = round(n / sum(n) * 100, 1),
         label = paste0(winner, "\n", n, " (", pct, "%)"))


# 4.7 statistical confirmation - wilcoxon + chi-square 

# paired Wilcoxon on the composite scores, for each diet pair, Bonferroni x3.
# no pivot needed - just line the two diets up by participant and pair them.
# H0: no difference in composite scores between diets.

pairs_list <- list(c("AUS", "MED"), c("AUS", "LC"), c("MED", "LC"))

wilcox_composite <- data.frame()
for (pr in pairs_list) {
  d1 <- composite %>% filter(diet == pr[1]) %>% arrange(participant)
  d2 <- composite %>% filter(diet == pr[2]) %>% arrange(participant)

  common <- intersect(d1$participant, d2$participant)
  x <- d1 %>% filter(participant %in% common) %>% arrange(participant) %>% pull(score)
  y <- d2 %>% filter(participant %in% common) %>% arrange(participant) %>% pull(score)
  n <- min(length(x), length(y))
  x <- x[1:n]; y <- y[1:n]

  w     <- wilcox.test(x, y, paired = TRUE, exact = FALSE, conf.int = TRUE)
  p_adj <- min(w$p.value * 3, 1)
  z     <- qnorm(w$p.value / 2)
  r     <- round(abs(z) / sqrt(n), 3)

  # effect size label
  if (r >= 0.5)      eff <- "Large"
  else if (r >= 0.3) eff <- "Medium"
  else if (r >= 0.1) eff <- "Small"
  else               eff <- "Negligible"

  wilcox_composite <- rbind(wilcox_composite, data.frame(
    Comparison   = paste(pr[1], "vs", pr[2]),
    N_pairs      = n,
    W            = round(as.numeric(w$statistic), 1),
    median_diff  = round(median(x - y), 3),
    p_value      = round(w$p.value, 4),
    p_adjusted   = round(p_adj, 4),
    p_label      = ifelse(p_adj < 0.001, "<0.001", as.character(round(p_adj, 4))),
    effect_r     = r,
    effect_label = eff,
    Significant  = ifelse(p_adj < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE))}

# chi-square goodness of fit: is each diet equally likely to win outright?
# H0: all diets equally likely to be best (expected = 1/3).
# run on participants with a clear (non-tied) winner only - a tie isn't a
# "win" for either diet, so including it as a fourth category would answer
# a different question. Ties are reported separately instead.
# MED rarely wins so expected counts are small -> use a Monte Carlo p-value.

clear_winners <- margins %>% filter(!is_tied)
n_tied        <- sum(margins$is_tied)

observed   <- table(factor(clear_winners$winner, levels = c("AUS", "MED", "LC")))
chi_result <- chisq.test(observed, simulate.p.value = TRUE, B = 10000)
chi_interpretation <- ifelse(chi_result$p.value < 0.05,
                              "Significant - winner distribution is not by chance",
                              "Not significant - could be by chance")

# binomial test per diet: does that one diet win more than 1/3 of the time?
# denominator is participants with a clear winner (ties excluded, since a tie
# is not a win for either diet in the comparison).
n_total <- nrow(clear_winners)
binom_results <- data.frame()
for (d in c("LC", "AUS", "MED")) {
  n_wins <- sum(clear_winners$winner == d)
  b_test <- binom.test(n_wins, n_total, p = 1/3, alternative = "greater")
  binom_results <- rbind(binom_results, data.frame(
    diet        = d,
    n_wins      = n_wins,
    n_total     = n_total,
    p_value     = round(b_test$p.value, 4),
    significant = ifelse(b_test$p.value < 0.05, "Significant", "Not significant")))}


# 4.8 best diet table (plot 11) 
# one row per participant (P1 top -> P23 bottom): the winning diet, the three
# metric values, whether the metrics agree, and a confidence flag.


best_vals <- bind_rows(
  # clear winners: same as before - one row from their winning diet
  composite %>%
    inner_join(margins %>% filter(!is_tied) %>% select(participant, winner),
               by = "participant") %>%
    filter(diet == winner) %>%
    group_by(participant) %>% slice(1) %>% ungroup(),

  # tied participants: no single winning diet, so show the average of the
  # tied diets' values (neither is uniquely better - that's the point)
  composite %>%
    inner_join(margins %>% filter(is_tied) %>% select(participant, winner),
               by = "participant") %>%
    rowwise() %>%
    filter(diet %in% strsplit(gsub("Tied: ", "", winner), "/")[[1]]) %>%
    ungroup() %>%
    group_by(participant, winner) %>%
    summarise(across(c(mean_glucose, cv_glucose, MAGE), mean), .groups = "drop")
) %>%
  left_join(margins %>% select(participant, win_margin, clarity, agreement),
            by = "participant") %>%
  mutate(
    lbl_glucose = round(mean_glucose, 2),
    lbl_cv      = round(cv_glucose,   1),
    lbl_mage    = round(MAGE,         3),
    confidence  = case_when(
      clarity == "Tied"                ~ "Tied",
      agreement == "All 3 agree"       ~ "High",
      agreement == "2 of 3 agree"      ~ "Moderate",
      agreement == "Composite only"    ~ "Low"),
    conf_colour = case_when(
      confidence == "High"     ~ "#1a7a4a",
      confidence == "Moderate" ~ "#E88B4C",
      confidence == "Low"      ~ "#E63946",
      confidence == "Tied"     ~ "#7F8C8D"
    ),
    final_bg = case_when(
      confidence == "Tied" ~ "#7F8C8D",
      confidence == "Low"  ~ "#E63946",
      winner == "LC"       ~ "#2A9D8F",
      winner == "MED"      ~ "#E88B4C",
      winner == "AUS"      ~ "#4C9BE8",
      TRUE                 ~ "#BDC3C7"))

p_table <- ggplot(best_vals, aes(y = order_participants(participant))) +
  geom_tile(aes(x = 3.5, fill = final_bg),
            width = 7.2, height = 0.85, alpha = 0.10) +
  geom_label(aes(x = 0, label = winner, fill = final_bg),
             colour = "white", fontface = "bold",
             size = 5.5, label.size = 0,
             label.padding = unit(0.3, "lines")) +
  geom_text(aes(x = 1.5, label = paste0(lbl_glucose, " mmol/L")),
            size = 4.3, colour = "grey20", fontface = "bold") +
  geom_text(aes(x = 3.0, label = paste0(lbl_cv, "%")),
            size = 4.3, colour = "grey20", fontface = "bold") +
  geom_text(aes(x = 4.5, label = paste0(lbl_mage, " mmol/L")),
            size = 4.3, colour = "grey20", fontface = "bold") +
  geom_text(aes(x = 6.1, label = agreement, colour = agreement),
            size = 4.0, fontface = "bold") +
  geom_label(aes(x = 7.6, label = winner, fill = final_bg),
             colour = "white", fontface = "bold",
             size = 5.0, label.size = 0,
             label.padding = unit(0.25, "lines")) +
  geom_label(aes(x = 7.6, label = confidence, fill = conf_colour),
             colour = "white", fontface = "bold",
             size = 3.6, label.size = 0,
             nudge_y = -0.32,
             label.padding = unit(0.12, "lines")) +
  scale_fill_manual(values = c(
    "#1a7a4a" = "#1a7a4a", "#2A9D8F" = "#2A9D8F", "#7F8C8D" = "#7F8C8D",
    "#4C9BE8" = "#4C9BE8", "#E88B4C" = "#E88B4C", "#E63946" = "#E63946")) +
  scale_colour_manual(values = c(
    "All 3 agree"    = "#1a7a4a",
    "2 of 3 agree"   = "#E88B4C",
    "Composite only" = "#E63946"
  )) +
  scale_y_discrete(limits = rev) +   # P1 at top, P23 at bottom
  # column headers, drawn just above the top row
  annotate("text", x = 0,   y = 24.3, label = "Best Diet",        fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("text", x = 1.5, y = 24.3, label = "Mean Glucose",     fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("text", x = 3.0, y = 24.3, label = "CV%",              fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("text", x = 4.5, y = 24.3, label = "MAGE",             fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("text", x = 6.1, y = 24.3, label = "Metric Agreement", fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("text", x = 7.6, y = 24.3, label = "Recommendation",   fontface = "bold", size = 4.6, colour = "grey20") +
  annotate("segment", x = -0.6, xend = 8.4, y = 23.7, yend = 23.7, colour = "grey50", linewidth = 0.8) +
  coord_cartesian(xlim = c(-0.8, 8.6), ylim = c(0.4, 24.8), clip = "off") +
  labs(
    subtitle = paste0(
      "Composite rank: Mean Glucose + CV% + MAGE (equal weight)\n",
      "Confidence: High = all 3 agree | Moderate = 2 of 3 | Low = composite only"
    ),
    x = "", y = "Participant"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.subtitle   = element_text(hjust = 0.5, colour = "grey30", size = 13, face = "bold"),
    legend.position = "none",
    axis.text.x     = element_blank(),
    axis.ticks.x    = element_blank(),
    panel.grid      = element_blank(),
    axis.text.y     = element_text(size = 13, face = "bold"))

ggsave("plot_11_best_diet_table.png", p_table, width = 14, height = 13, dpi = 300, bg = "white")


# 4.9 save 

write_xlsx(
  list(
    "Responder_Detail" = as.data.frame(
      response_data %>%
        select(participant, diet, Sex, AgeGrp, PreS.BMI,
               grand_mean_glucose, mean_glucose, pct_change_glucose, glucose_category,
               grand_mean_cv, cv_glucose, diff_cv_pts, cv_category,
               grand_mean_mage, MAGE, diff_mage_mmol, mage_category) %>%
        arrange(diet, pct_change_glucose)),
    "Composite_Rankings" = as.data.frame(
      composite %>%
        select(participant, diet, mean_glucose, cv_glucose, MAGE,
               r_glucose, r_cv, r_mage, score) %>%
        arrange(participant, score)
    ),
    "Best_Diet_Summary" = as.data.frame(
      margins %>%
        select(participant, winner,
               Margin = win_margin, Clarity = clarity, Agreement = agreement) %>%
        arrange(winner, participant)
    ),
    "Winner_Counts"      = as.data.frame(win_counts),
    "Wilcoxon_Composite" = as.data.frame(wilcox_composite),
    "ChiSquare_BestDiet" = data.frame(
      Test      = "Chi-square",
      Statistic = round(chi_result$statistic, 3),
      p_value   = round(chi_result$p.value, 4),
      Interpretation = chi_interpretation),
    "Binomial_BestDiet"  = binom_results),
  "CGM_Individual_Response.xlsx")
