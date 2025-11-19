# Evaluation of Temporal Trends in Physical Activity Among US Adults, 2020-2024
# Rishi M. Shah
# 18 November 2025

# import libraries
library(dplyr)
library(survey)
library(srvyr)
library(data.table)
library(ggsci)
library(ggpubr)
library(boot)
library(patchwork)

# set the seed for reproducibility
set.seed(20251118)

# data pre-processing

# load data in
df <- fread('nhis_exercise_20-24.csv')

names(df) <- tolower(names(df))

# only need sample adults
df <- subset(df, astatflg == 1)

# adjust respondent-level weights to incorporate the number of years included in analysis
# for this analysis, we do not need to divide by years because we are not pooling, bur rather looking at each year individually
# https://nhis.ipums.org/nhis/userNotes_variance.shtml
df$sampweight_pooled <- df$sampweight # / nyears
df$strata_pooled <- df$strata
df$psu_pooled <- df$psu

# outcomes

# 2018 HHS physcial activity guidelines
df$noexercise <- ifelse(df$pa18aerstr == 1, 1, 
                        ifelse(df$pa18aerstr %in% c(2, 3, 4), 0, NA))

df$strength <- ifelse(df$pa18aerstr == 2, 1, 
                      ifelse(df$pa18aerstr %in% c(1, 3, 4), 0, NA))

df$aerobic <- ifelse(df$pa18aerstr == 3, 1, 
                     ifelse(df$pa18aerstr %in% c(1, 2, 4), 0, NA))

df$bothexercise <- ifelse(df$pa18aerstr == 4, 1, 
                          ifelse(df$pa18aerstr %in% c(1, 2, 3), 0, NA))

# covariates

# race/ethnicity
df$race_simple <- NA
df$race_simple[df$hisprace == 1] <- 1
df$race_simple[df$hisprace == 2] <- 2
df$race_simple[df$hisprace == 3] <- 3
df$race_simple[df$hisprace == 4] <- 4
df$race_simple[df$hisprace >= 5 & df$hisprace <= 6] <- 5
df$race_simple[df$hisprace == 7] <- 6

levels <- c("Hispanic", "NH White", "NH Black", "NH Asian", "NH AIAN", "NH Other")
labels <- c(1, 2, 3, 4, 5, 6)
df$race_simple <- factor(df$race_simple, levels = labels, labels = levels)

df$white <- ifelse(df$race_simple == "NH White", 1, 0)
df$black <- ifelse(df$race_simple == "NH Black", 1, 0)
df$asian <- ifelse(df$race_simple == "NH Asian", 1, 0)
df$aian <- ifelse(df$race_simple == "NH AIAN", 1, 0)
df$otherrace <- ifelse(df$race_simple == "NH Other", 1, 0)
df$hispanic <- ifelse(df$race_simple == "Hispanic", 1, 0)

# binarize sex, 1 = female, 0 = male
df$sex <- ifelse(df$sex %in% c(7, 9), NA, ifelse(df$sex == 1, 0, 1))

# binarize less than or greater than high school education
# 1 = some college or higher
df$educ_binary <- NA
df$educ_binary[df$educ >= 100 & df$educ < 300] <- 0
df$educ_binary[df$educ >= 300 & df$educ < 990] <- 1

levels <- c("Less HS", "More HS")
labels <- c(0, 1)
df$educ_binary <- factor(df$educ_binary, levels = labels, labels = levels)

# create income groups based on ratio of family income to federal poverty threshold
df <- df %>%
  mutate(
    income = case_when(
      poverty %in% c(11, 12, 13) ~ "<100% FPL",
      poverty %in% c(21, 22, 23, 24) ~ "100–199% FPL",
      poverty %in% c(31, 32) ~ "200–299% FPL",
      poverty %in% c(33, 34) ~ "300–399% FPL",
      poverty %in% c(35, 36) ~ "400-499% FPL",
      poverty == 37 ~ "500%+ FPL",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    income = factor(
      income,
      levels = c("<100% FPL", "100–199% FPL", "200–299% FPL", "300–399% FPL", "400-499% FPL", "500%+ FPL")
    )
  )

# make three age categories
df$age <- ifelse(df$age >= 997, NA, df$age)

df$agecat <- NA
df$agecat[df$age < 40] <- 1
df$agecat[df$age >= 40 & df$age < 65] <- 2
df$agecat[df$age >= 65 & df$age <= 99] <- 3

levels <- c("< 40 years", "40-64 years", "≥ 65 years")
labels <- c(1, 2, 3)
df$agecat <- factor(df$agecat, levels = labels, labels = levels)

# make binary variables for each region of the US
df$neast <- ifelse(df$region == 1, 1, 0)
df$midwest <- ifelse(df$region == 2, 1, 0)
df$south <- ifelse(df$region == 3, 1, 0)
df$west <- ifelse(df$region == 4, 1, 0)

levels <- c("Northeast", "Midwest", "South", "West")
labels <- c(1, 2, 3, 4)
df$region <- factor(df$region, levels = labels, labels = levels)

# urban/rural classification
df$lrge_cent_metr <- ifelse(df$urbrrl == 1, 1, 0)
df$lrge_frin_metr <- ifelse(df$urbrrl == 2, 1, 0)
df$med_sml_metr <- ifelse(df$urbrrl == 3, 1, 0)
df$non_metr <- ifelse(df$urbrrl == 4, 1, 0)

df$urbrur <- ifelse(df$urbrrl %in% c(1, 2, 3), 1, ifelse(df$urbrrl == 4, 0, NA))
# levels <- c("Nonmetropolitan", "Metropolitan")
# labels <- c(0, 1)
# df$urbrur <- factor(df$urbrur, levels = labels, labels = levels)

# combordities

# hypertension
df$hypertension <- NA
df$hypertension <- ifelse(df$hypertenev == 2, 1, ifelse(df$hypertenev == 1, 0, df$hypertension))

# cardiovascular disease
df$cvd <- NA
df$cvd[df$strokev == 1 & df$angipecev == 1 & df$heartattev == 1 & df$cheartdiev == 1] <- 0
df$cvd[df$strokev == 2 | df$angipecev == 2 | df$heartattev == 2 | df$cheartdiev == 2] <- 1

# figure 1: trends in physical activity by ascvd status
options(survey.lonely.psu = "adjust")
svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

outcomes <- c("noexercise", "strength", "aerobic", "bothexercise")

results_exercise <- data.frame(outcome = character(), ascvd_group = character(), year = integer(), 
                               rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                               stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (yr in c(2020, 2022, 2024)) {
    svy_yr <- subset(svy_design, year == yr & !is.na(get(outcome)) & !is.na(cvd))
    
    overall_prev <- svymean(as.formula(paste0("~", outcome)), svy_yr, na.rm = TRUE)
    overall_ci <- confint(overall_prev)
    
    results_exercise <- rbind(results_exercise, data.frame(
      outcome = outcome,
      ascvd_group = "Overall",
      year = yr,
      rate = as.numeric(overall_prev),
      stderr = as.numeric(SE(overall_prev)),
      lb = overall_ci[1],
      ub = overall_ci[2]
    ))
    
    svy_no_cvd <- subset(svy_yr, cvd == 0)
    no_cvd_prev <- svymean(as.formula(paste0("~", outcome)), svy_no_cvd, na.rm = TRUE)
    no_cvd_ci <- confint(no_cvd_prev)
    
    results_exercise <- rbind(results_exercise, data.frame(
      outcome = outcome,
      ascvd_group = "No ASCVD",
      year = yr,
      rate = as.numeric(no_cvd_prev),
      stderr = as.numeric(SE(no_cvd_prev)),
      lb = no_cvd_ci[1],
      ub = no_cvd_ci[2]
    ))
    
    svy_cvd <- subset(svy_yr, cvd == 1)
    cvd_prev <- svymean(as.formula(paste0("~", outcome)), svy_cvd, na.rm = TRUE)
    cvd_ci <- confint(cvd_prev)
    
    results_exercise <- rbind(results_exercise, data.frame(
      outcome = outcome,
      ascvd_group = "ASCVD",
      year = yr,
      rate = as.numeric(cvd_prev),
      stderr = as.numeric(SE(cvd_prev)),
      lb = cvd_ci[1],
      ub = cvd_ci[2]
    ))
  }
}

results_exercise$ascvd_group <- factor(results_exercise$ascvd_group, 
                                       levels = c("Overall", "No ASCVD", "ASCVD"))

write.csv(results_exercise, "exercise_ascvd_results_v1_111825.csv", row.names = FALSE)

outcome_labels <- c(
  "noexercise" = "Meeting neither aerobic nor\nstrengthening guidelines, %",
  "strength" = "Meeting strengthening guidelines only, %",
  "aerobic" = "Meeting aerobic guidelines only, %",
  "bothexercise" = "Meeting both aerobic and\nstrengthening guidelines, %"
)

make_exercise_panel <- function(outcome_name, show_legend, tag, ymax) {
  p <- ggplot(
    results_exercise %>% filter(outcome == outcome_name),
    aes(x = year, y = rate, color = ascvd_group, shape = ascvd_group, group = ascvd_group)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.15, linewidth = 0.7) +
    scale_color_jama(name = "ASCVD Status") +
    scale_shape_manual(name = "ASCVD Status", values = c(16, 17, 15)) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0.05, 0.05)),
      limits = c(0, ymax)
    ) +
    scale_x_continuous(breaks = c(2020, 2022, 2024)) +
    labs(
      x = "Year",
      y = outcome_labels[outcome_name],
      tag = tag
    ) +
    theme_pubr(base_size = 12) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 11),
      legend.title = element_text(face = "bold"),
      plot.tag = element_text(face = "bold", size = 14),
      plot.tag.position = c(0, 1)
    )
  
  if (show_legend) {
    p <- p + theme(
      legend.position = c(0.85, 0.90),
      legend.background = element_rect(fill = "white", color = "white")
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

pA <- make_exercise_panel("noexercise", show_legend = FALSE, tag = "A", ymax = 0.75)
pB <- make_exercise_panel("strength", show_legend = TRUE, tag = "B", ymax = 0.25)
pC <- make_exercise_panel("aerobic", show_legend = FALSE, tag = "C", ymax = 0.5)
pD <- make_exercise_panel("bothexercise", show_legend = FALSE, tag = "D", ymax = 0.5)

figure_1 <- (pA | pB) / (pC | pD)

ggsave('fig1_exercise_overall_111825_v1.pdf', figure_1, device = pdf, width = 10, height = 8)

# figure 2: trends in physical activity by race, income, and age
# center covariates by their study population mean
df$age <- df$age - mean(df$age, na.rm = TRUE)
df$neast <- df$neast - mean(df$neast, na.rm = TRUE)
df$midwest <- df$midwest - mean(df$midwest, na.rm = TRUE)
df$south <- df$south - mean(df$south, na.rm = TRUE)
df$west <- df$west - mean(df$west, na.rm = TRUE)
df$sex <- df$sex - mean(df$sex, na.rm = TRUE)
df$urbrur <- df$urbrur - mean(df$urbrur, na.rm = TRUE)
df$white <- df$white - mean(df$white, na.rm = TRUE)
df$black <- df$black - mean(df$black, na.rm = TRUE)
df$asian <- df$asian - mean(df$asian, na.rm = TRUE)
df$aian <- df$aian - mean(df$aian, na.rm = TRUE)
df$otherrace <- df$otherrace - mean(df$otherrace, na.rm = TRUE)
df$hispanic <- df$hispanic - mean(df$hispanic, na.rm = TRUE)
df$povlev <- df$povlev - mean(df$povlev, na.rm = TRUE)

# analysis

# set survey design
options(survey.lonely.psu = "adjust")
svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

# race
results_race <- data.frame(outcome = character(), race_group = character(), year = integer(), 
                           rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                           stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (race_group in levels(df$race_simple)) {
    temp <- subset(svy_design, race_simple == race_group)
    temp$variables$year <- as.factor(temp$variables$year)
    
    formula <- paste(outcome, "~ age + sex + neast + midwest + south + west + urbrur + povlev + year")
    
    fit <- svyglm(as.formula(formula), design = temp, family = quasibinomial(link = 'logit'))
    
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    subset_draws <- draws
    
    for (year in c(2020, 2022, 2024)) {
      if (year == 2020) {
        rates <- inv.logit(subset_draws[, "(Intercept)"])
      } else {
        coef_name <- paste0("year", year)
        if (coef_name %in% colnames(subset_draws)) {
          rates <- inv.logit(subset_draws[, "(Intercept)"] + subset_draws[, coef_name])
        } else {
          next
        }
      }
      
      rate_mean <- mean(rates, na.rm = TRUE)
      rate_sd <- sd(rates, na.rm = TRUE)
      rate_lb <- quantile(rates, 0.025, na.rm = TRUE)
      rate_ub <- quantile(rates, 0.975, na.rm = TRUE)
      
      results_race <- rbind(
        results_race,
        data.frame(
          outcome = outcome,
          race_group = race_group,
          year = year,
          rate = rate_mean,
          stderr = rate_sd,
          lb = rate_lb,
          ub = rate_ub,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

write.csv(results_race, file = "ratefile_exercise_race_111825_v1.csv", row.names = FALSE)

# income
results_income <- data.frame(outcome = character(), income_group = character(), year = integer(), 
                             rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                             stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (income_group in levels(df$income)) {
    temp <- subset(svy_design, income == income_group)
    temp$variables$year <- as.factor(temp$variables$year)
    
    formula <- paste(outcome, "~ age + sex + white + black + hispanic + asian + aian + otherrace + neast + midwest + south + west + urbrur + year")
    
    fit <- svyglm(as.formula(formula), design = temp, family = quasibinomial(link = 'logit'))
    
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    subset_draws <- draws
    
    for (year in c(2020, 2022, 2024)) {
      if (year == 2020) {
        rates <- inv.logit(subset_draws[, "(Intercept)"])
      } else {
        coef_name <- paste0("year", year)
        if (coef_name %in% colnames(subset_draws)) {
          rates <- inv.logit(subset_draws[, "(Intercept)"] + subset_draws[, coef_name])
        } else {
          next
        }
      }
      
      rate_mean <- mean(rates, na.rm = TRUE)
      rate_sd <- sd(rates, na.rm = TRUE)
      rate_lb <- quantile(rates, 0.025, na.rm = TRUE)
      rate_ub <- quantile(rates, 0.975, na.rm = TRUE)
      
      results_income <- rbind(
        results_income,
        data.frame(
          outcome = outcome,
          income_group = income_group,
          year = year,
          rate = rate_mean,
          stderr = rate_sd,
          lb = rate_lb,
          ub = rate_ub,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

write.csv(results_income, file = "ratefile_exercise_income_111825_v1.csv", row.names = FALSE)

# age
results_age <- data.frame(outcome = character(), age_group = character(), year = integer(), 
                          rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                          stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (age_group in levels(df$agecat)) {
    temp <- subset(svy_design, agecat == age_group)
    temp$variables$year <- as.factor(temp$variables$year)
    
    formula <- paste(outcome, "~ sex + white + black + hispanic + asian + aian + otherrace + neast + midwest + south + west + urbrur + povlev + year")
    
    fit <- svyglm(as.formula(formula), design = temp, family = quasibinomial(link = 'logit'))
    
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    subset_draws <- draws
    
    for (year in c(2020, 2022, 2024)) {
      if (year == 2020) {
        rates <- inv.logit(subset_draws[, "(Intercept)"])
      } else {
        coef_name <- paste0("year", year)
        if (coef_name %in% colnames(subset_draws)) {
          rates <- inv.logit(subset_draws[, "(Intercept)"] + subset_draws[, coef_name])
        } else {
          next
        }
      }
      
      rate_mean <- mean(rates, na.rm = TRUE)
      rate_sd <- sd(rates, na.rm = TRUE)
      rate_lb <- quantile(rates, 0.025, na.rm = TRUE)
      rate_ub <- quantile(rates, 0.975, na.rm = TRUE)
      
      results_age <- rbind(
        results_age,
        data.frame(
          outcome = outcome,
          age_group = age_group,
          year = year,
          rate = rate_mean,
          stderr = rate_sd,
          lb = rate_lb,
          ub = rate_ub,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

write.csv(results_age, file = "ratefile_exercise_age_111825_v1.csv", row.names = FALSE)

make_panel <- function(data, group_var, outcome_name, show_legend, tag, legend_title, ymax) {
  p <- ggplot(
    data %>% filter(outcome == outcome_name),
    aes_string(x = "year", y = "rate", color = group_var, shape = group_var, group = group_var)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.15, linewidth = 0.7) +
    scale_color_jama(name = legend_title) +
    scale_shape_manual(
      name = legend_title,
      values = c(16, 17, 15, 18, 8, 3)[seq_along(unique(pull(data, !!sym(group_var))))]
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0)),
      limits = c(0, ymax),
    ) +
    scale_x_continuous(breaks = c(2020, 2022, 2024)) +
    labs(
      x = "Year",
      y = outcome_labels[outcome_name],
      tag = tag
    ) +
    theme_pubr(base_size = 12) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.title.y = element_text(size = 11),
      legend.title = element_text(face = "bold"),
      plot.tag = element_text(face = "bold", size = 14),
      plot.tag.position = c(0, 1)
    )
  
  if (show_legend) {
    p <- p + theme(legend.position = "right")
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

results_age <- results_age %>%
  mutate(age_group = factor(age_group, levels = c("< 40 years", "40-64 years", "\u2265 65 years")))

# only plotting age and income
pA <- make_panel(results_income, "income_group", "noexercise", FALSE, "A", "Family Income", ymax = 0.75)
pB <- make_panel(results_income, "income_group", "strength", FALSE, "B", "Family Income", ymax = 0.25)
pC <- make_panel(results_income, "income_group", "aerobic", FALSE, "C", "Family Income", ymax = 0.5)
pD <- make_panel(results_income, "income_group", "bothexercise", TRUE, "D", "Family Income", ymax = 0.5)

pE <- make_panel(results_age, "age_group", "noexercise", FALSE, "E", "Age", ymax = 0.75)
pF <- make_panel(results_age, "age_group", "strength", FALSE, "F", "Age", ymax = 0.25)
pG <- make_panel(results_age, "age_group", "aerobic", FALSE, "G", "Age", ymax = 0.5)
pH <- make_panel(results_age, "age_group", "bothexercise", TRUE, "H", "Age", ymax = 0.5)

figure_2 <- (pA | pB | pC | pD) / (pE | pF | pG | pH)

ggsave('fig2_exercise_race_income_age_111825_v1.jpeg', figure_2, device = jpeg, width = 16, height = 8)



