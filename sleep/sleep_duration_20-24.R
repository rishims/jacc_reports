# Evaluation of Temporal Trends in Sleep Duration Among US Adults, 2020-2024
# Rishi M. Shah
# 06 November 2025

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
set.seed(20251106)

# data pre-processing

# load data in
df <- fread('/sleep/nhis_sleep_20-24.csv')

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

# hours of sleep each day
df$hrsleep <- ifelse(df$hrsleep >= 97, NA, df$hrsleep)
df$shortsleep <- ifelse(df$hrsleep < 7, 1, 0)
df$longsleep <- ifelse(df$hrsleep > 9, 1, 0)
df$enoughsleep <- ifelse(df$hrsleep >= 7 , 1, 0)

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


# figure 1 average sleep duration by ASCVD status
df_plot <- df %>%
  filter(!is.na(hrsleep) & !is.na(cvd)) %>%
  mutate(
    cvd_status = case_when(
      cvd == 1 ~ "ASCVD",
      cvd == 0 ~ "No ASCVD",
      TRUE ~ NA_character_
    ),
    year_fct = factor(year)
  )

df_overall <- df_plot %>%
  mutate(cvd_status = "Overall")

df_combined <- bind_rows(df_plot, df_overall) %>%
  mutate(cvd_status = factor(cvd_status, levels = c("Overall", "No ASCVD", "ASCVD")))

svy_design <- svydesign(
  id = ~psu_pooled,
  strata = ~strata_pooled,
  weights = ~sampweight_pooled,
  data = df_combined,
  nest = TRUE
)

quantiles_list <- list()
means_list <- list()

for(yr in c(2020, 2022, 2024)) {
  for(status in c("Overall", "No ASCVD", "ASCVD")) {
    svy_sub <- subset(svy_design, year == yr & cvd_status == status)
    
    q <- svyquantile(~hrsleep, svy_sub, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), ci = FALSE)
    
    mean_est <- svymean(~hrsleep, svy_sub, na.rm = TRUE)
    
    quantiles_list[[length(quantiles_list) + 1]] <- data.frame(
      year = yr,
      cvd_status = status,
      q05 = as.numeric(q[[1]][1]),
      q25 = as.numeric(q[[1]][2]),
      median = as.numeric(q[[1]][3]),
      q75 = as.numeric(q[[1]][4]),
      q95 = as.numeric(q[[1]][5])
    )
    
    means_list[[length(means_list) + 1]] <- data.frame(
      year = yr,
      cvd_status = status,
      mean = as.numeric(mean_est),
      se = as.numeric(SE(mean_est))
    )
  }
}

quantiles_df <- bind_rows(quantiles_list) %>%
  mutate(
    cvd_status = factor(cvd_status, levels = c("Overall", "No ASCVD", "ASCVD")),
    year_fct = factor(year)
  )

means_df <- bind_rows(means_list) %>%
  mutate(
    cvd_status = factor(cvd_status, levels = c("Overall", "No ASCVD", "ASCVD")),
    year_fct = factor(year),
    lower_95ci = mean - 1.96 * se,
    upper_95ci = mean + 1.96 * se
  )

positions <- quantiles_df %>%
  group_by(year) %>%
  mutate(
    x_pos = as.numeric(year_fct) + (as.numeric(cvd_status) - 2) * 0.25
  ) %>%
  ungroup()

means_positions <- means_df %>%
  group_by(year) %>%
  mutate(
    x_pos = as.numeric(year_fct) + (as.numeric(cvd_status) - 2) * 0.25
  ) %>%
  ungroup()

positions <- left_join(positions, means_positions, by = c("year", "cvd_status", "year_fct", "x_pos"))

positions <- positions %>%
  mutate(
    label = sprintf("%.1f±%.2f", mean, se),
    y_pos = (q25 + q75) / 2
  )

summary_stats <- positions %>%
  select(year, cvd_status, q05, q25, median, q75, q95, mean, se, lower_95ci, upper_95ci) %>%
  arrange(year, cvd_status)

write.csv(summary_stats, "/sleep/sleep_cvd_summary_stats.csv", row.names = FALSE)

fig1 <- ggplot(positions, aes(x = x_pos, group = interaction(year_fct, cvd_status), fill = cvd_status)) +
  geom_boxplot(
    aes(ymin = q05, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    alpha = 0.7,
    color = "black",
    linewidth = 0.4,
    fatten = 1.5
  ) +
  geom_errorbar(
    aes(ymin = q05, ymax = q05, x = x_pos),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  geom_errorbar(
    aes(ymin = q95, ymax = q95, x = x_pos),
    width = 0.15,
    linewidth = 0.4,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = 1:3,
    labels = c("2020", "2022", "2024")
  ) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = seq(0, 12, 2)
  ) +
  scale_fill_jama() +
  labs(
    x = "Year",
    y = "Average sleep duration (hours/day)",
    fill = "ASCVD Status"
  ) +
  theme_pubr(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "white", color = "white"),
    legend.title = element_text(face = "bold")
  )

ggsave('/sleep/fig1_sleep_overall_111225_v1.pdf', fig1, device = 'pdf', width = 10, height = 8)

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

# define the outcomes
outcomes <- c("shortsleep", "enoughsleep", "longsleep")

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

write.csv(results_race, file = "/sleep/ratefile_sleep_race_111225.csv", row.names = FALSE)

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

write.csv(results_income, file = "/sleep/ratefile_sleep_income_111225.csv", row.names = FALSE)


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

write.csv(results_age, file = "/sleep/ratefile_sleep_age_111225.csv", row.names = FALSE)

make_plot <- function(data, group_var, outcome_filter, y_label, title, tag, show_legend = FALSE, legend_title = NULL, y_min = NULL, y_max = NULL) {
  p <- ggplot(
    data %>% filter(outcome == outcome_filter),
    aes_string(x = "year", y = "rate", color = group_var, shape = group_var, group = group_var)
  ) +
    geom_line(size = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.1, linewidth = 0.7) +
    scale_color_jama(name = legend_title) +
    scale_shape_manual(
      name = legend_title,
      values = c(16, 17, 15, 18, 8, 3)[seq_along(unique(pull(data, !!sym(group_var))))]
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1), 
      expand = expansion(mult = c(0, 0.05)),
      limits = c(y_min, y_max)
    ) +
    scale_x_continuous(breaks = c(2020, 2022, 2024)) +
    labs(
      x = "Year",
      y = y_label,
      title = title,
      tag = tag
    ) +
    theme_pubr(base_size = 14) +
    theme(
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.key = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      plot.tag = element_text(face = "bold", size = 16, hjust = 0.5, vjust = 1),
      plot.tag.position = c(0, 1),
      legend.title = element_text(face = "bold")
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

pA <- make_plot(results_race, "race_group", "shortsleep",
                "% Reporting < 7 hrs of sleep",
                "", "A", show_legend = FALSE, legend_title = "Race/Ethnicity", y_min = 0.2, y_max = 0.6)
pB <- make_plot(results_race, "race_group", "longsleep",
                "% Reporting > 9 hrs of sleep",
                "", "B", show_legend = TRUE, legend_title = "Race/Ethnicity", y_min = 0, y_max = 0.1)
pC <- make_plot(results_income, "income_group", "shortsleep",
                "% Reporting < 7 hrs of sleep",
                "", "C", show_legend = FALSE, legend_title = "Family Income", y_min = 0.2, y_max = 0.6)
pD <- make_plot(results_income, "income_group", "longsleep",
                "% Reporting > 9 hrs of sleep",
                "", "D", show_legend = TRUE, legend_title = "Family Income", y_min = 0, y_max = 0.1)
pE <- make_plot(results_age, "age_group", "shortsleep",
                "% Reporting < 7 hrs of sleep",
                "", "E", show_legend = FALSE, legend_title = "Age", y_min = 0.2, y_max = 0.6)
pF <- make_plot(results_age, "age_group", "longsleep",
                "% Reporting > 9 hrs of sleep",
                "", "F", show_legend = TRUE, legend_title = "Age", y_min = 0, y_max = 0.1)

fig2 <- (pA | pB) /
  (pC | pD) /
  (pE | pF)

ggsave('/sleep/fig2_short_long_sleep_111225_v1.jpeg', fig2, device = jpeg, width = 15, height = 12)


 

