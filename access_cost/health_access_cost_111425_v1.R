# Trends in Health Access, Affordability, and Utilization among US Adults, 2019-2024
# Rishi M. Shah
# 04 November 2025

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
set.seed(20251104)

# data pre-processing

# load data in
startyr <- 2019
endyr <- 2024
nyears <- endyr - startyr + 1
df <- fread('nhis_19-24.csv')

names(df) <- tolower(names(df))

# only need sample adults
df <- subset(df, astatflg == 1)

# first adjust 2020 survey weights because telehealth was only ascertained in quarters 3 and 4
# https://nhis.ipums.org/nhis/userNotes_weights.shtml
df$telehealth_weights <- ifelse(df$year == 2020, df$sampweight * 2, df$sampweight)

# adjust respondent-level weights for 2019 and 2020 due to longitudinal sample
# https://nhis.ipums.org/nhis/userNotes_weights.shtml
df <- subset(df, partweight != 0 | is.na(partweight))
df$sampweight <- ifelse(df$year == 2020, df$partweight, df$sampweight)

# adjust respondent-level weights to incorporate the number of years included in analysis
# for this analysis, we do not need to divide by years because we are not pooling, bur rather looking at each year individually
# https://nhis.ipums.org/nhis/userNotes_variance.shtml
df$sampweight_pooled <- df$sampweight # / nyears
df$strata_pooled <- df$strata
df$psu_pooled <- df$psu

# outcomes
# each IPUMS variable: 2 = yes, 1 = no
# new variable: 1 = yes

# no insurance coverage
df$noinsurance <- ifelse(df$hinotcove == 1, 0, ifelse(df$hinotcove == 2, 1, NA))

# foregone or delayed medical care due to cost
df$nocarecost <- ifelse(df$ybarcare == 1, 0, ifelse(df$ybarcare == 2, 1, NA))
df$latecarecost <- ifelse(df$delaycost == 1, 0, ifelse(df$delaycost == 2, 1, NA))
df$badcarecost <- ifelse(df$nocarecost == 0 & df$latecarecost == 0, 0, ifelse(df$nocarecost == 1 | df$latecarecost == 1, 1, NA))

# foregone or delayed prescription medications due to cost
df$skipped_meds <- ifelse(df$yskipmedyr == 1, 0, ifelse(df$yskipmedyr == 2, 1, NA))
df$less_meds <- ifelse(df$yskimpmedyr == 1, 0, ifelse(df$yskimpmedyr == 2, 1, NA))
df$delayed_meds <- ifelse(df$ydelaymedyr == 1, 0, ifelse(df$ydelaymedyr == 2, 1, NA))
df$cost_barrier <- ifelse(df$ybarmeds == 1, 0, ifelse(df$ybarmeds == 2, 1, NA))

df$crn_barrier <- NA
df$crn_barrier[df$skipped_meds == 0 & df$less_meds == 0 & df$delayed_meds == 0 & df$cost_barrier == 0] <- 0
df$crn_barrier[df$skipped_meds == 1 | df$less_meds == 1 | df$delayed_meds == 1 | df$cost_barrier == 1] <- 1

# no usual source of care
df$noplace <- ifelse(df$usualpl %in% c(2, 3), 0, ifelse(df$usualpl == 1 | df$typplsick == 200, 1, NA))

# not seen/talked to a health care professional in the past year
df$novisit <- ifelse(df$dvint >= 200 & df$dvint < 300, 0, ifelse(df$dvint == 100 | (df$dvint >= 300 & df$dvint < 997), 1, NA))

# telehealth, only for 2020-2024
df$telehealth <- ifelse(df$virapp12m == 1, 0, ifelse(df$virapp12m == 2, 1, NA))

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

# figure 1: trends in access and affordability by ascvd status and year
options(survey.lonely.psu = "adjust")
svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

outcomes <- c("noinsurance", "badcarecost", "crn_barrier", "noplace")

results <- data.frame(outcome = character(), cvd_group = character(), year = integer(), 
                      rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                      stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (yr in 2019:2024) {
    svy_yr <- subset(svy_design, year == yr & !is.na(get(outcome)) & !is.na(cvd))
    
    overall_prev <- svymean(as.formula(paste0("~", outcome)), svy_yr, na.rm = TRUE)
    overall_ci <- confint(overall_prev)
    
    results <- rbind(results, data.frame(
      outcome = outcome,
      cvd_group = "Overall",
      year = yr,
      rate = as.numeric(overall_prev),
      stderr = as.numeric(SE(overall_prev)),
      lb = overall_ci[1],
      ub = overall_ci[2]
    ))
    
    svy_no_cvd <- subset(svy_yr, cvd == 0)
    no_cvd_prev <- svymean(as.formula(paste0("~", outcome)), svy_no_cvd, na.rm = TRUE)
    no_cvd_ci <- confint(no_cvd_prev)
    
    results <- rbind(results, data.frame(
      outcome = outcome,
      cvd_group = "No ASCVD",
      year = yr,
      rate = as.numeric(no_cvd_prev),
      stderr = as.numeric(SE(no_cvd_prev)),
      lb = no_cvd_ci[1],
      ub = no_cvd_ci[2]
    ))
    
    svy_cvd <- subset(svy_yr, cvd == 1)
    cvd_prev <- svymean(as.formula(paste0("~", outcome)), svy_cvd, na.rm = TRUE)
    cvd_ci <- confint(cvd_prev)
    
    results <- rbind(results, data.frame(
      outcome = outcome,
      cvd_group = "ASCVD",
      year = yr,
      rate = as.numeric(cvd_prev),
      stderr = as.numeric(SE(cvd_prev)),
      lb = cvd_ci[1],
      ub = cvd_ci[2]
    ))
  }
}

results$cvd_group <- factor(results$cvd_group, levels = c("Overall", "No ASCVD", "ASCVD"))

write.csv(results, "health_access_cvd_results_111325_v1.csv", row.names = FALSE)

outcome_labels <- c(
  "noinsurance" = "Uninsured, %",
  "badcarecost" = "Delayed or forwent medical\ncare due to cost in the past year, %",
  "crn_barrier" = "Experienced cost-related medication\nnonadherence in the past year, %",
  "noplace" = "Without a usual source of care, %"
)

make_panel <- function(outcome_name, show_legend, tag) {
  p <- ggplot(
    results %>% filter(outcome == outcome_name),
    aes(x = year, y = rate, color = cvd_group, shape = cvd_group, group = cvd_group)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.15, linewidth = 0.7) +
    scale_color_jama(name = "ASCVD Status") +
    scale_shape_manual(name = "ASCVD Status", values = c(16, 17, 15)) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0.05, 0.05)),
      limits = c(0, 0.2)
    ) +
    scale_x_continuous(breaks = 2019:2024) +
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
      legend.position = c(0.95, 0.9),
      legend.background = element_rect(fill = "white", color = "white")
    )
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}

pA_fig1 <- make_panel("noinsurance", show_legend = TRUE, tag = "A")
pB_fig1 <- make_panel("noplace", show_legend = FALSE, tag = "B")

figure1 <- pA_fig1 / pB_fig1

pA_fig2 <- make_panel("crn_barrier", show_legend = TRUE, tag = "A")
pB_fig2 <- make_panel("badcarecost", show_legend = FALSE, tag = "B")

figure2 <- pA_fig2 / pB_fig2

ggsave('fig1_access_overall_111325_v1.pdf', figure1, device = pdf, width = 10, height = 8)
ggsave('fig1_cost_overall_111325_v1.pdf', figure2, device = pdf, width = 10, height = 8)

# figure 2: access and affordability by race
# exclude NH Other due to low counts
df <- df %>%
  filter(race_simple != "NH Other")

# center covariates by their study population mean
df$age <- df$age - mean(df$age, na.rm = TRUE)
df$neast <- df$neast - mean(df$neast, na.rm = TRUE)
df$midwest <- df$midwest - mean(df$midwest, na.rm = TRUE)
df$south <- df$south - mean(df$south, na.rm = TRUE)
df$west <- df$west - mean(df$west, na.rm = TRUE)
df$sex <- df$sex - mean(df$sex, na.rm = TRUE)
df$urbrur <- df$urbrur - mean(df$urbrur, na.rm = TRUE)
df$povlev <- df$povlev - mean(df$povlev, na.rm = TRUE)

options(survey.lonely.psu = "adjust")
svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

outcomes <- c("noinsurance", "badcarecost", "crn_barrier", "noplace")

results_race <- data.frame(outcome = character(), race_group = character(), year = integer(), 
                           rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                           stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (race_group in levels(df$race_simple)) {
    if (race_group == "NH Other") next
    
    temp <- subset(svy_design, race_simple == race_group & cvd == 1)
    temp$variables$year <- as.factor(temp$variables$year)
    formula <- paste(outcome, "~ age + sex + neast + midwest + south + west + urbrur + povlev + year")
    
    fit <- svyglm(as.formula(formula), design = temp, family = quasibinomial(link = 'logit'))
    
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    coef_names <- names(b)
    coef_indices <- match(coef_names, colnames(draws))
    
    subset_draws <- draws[, coef_indices]
    
    for (year in 2019:2024) {
      n <- year - 2020 + 9
      
      if(year == 2019) {
        rates <- inv.logit(subset_draws[, 1])
      } else {
        rates <- inv.logit(subset_draws[, n] + subset_draws[, 1])
      }
      
      rate_mean <- mean(rates)
      rate_sd <- sd(rates)
      rate_lb <- quantile(rates, 0.025)
      rate_ub <- quantile(rates, 0.975)
      
      results_race <- rbind(results_race, data.frame(
        outcome = outcome, race_group = race_group, year = year, rate = rate_mean,
        stderr = rate_sd, lb = rate_lb, ub = rate_ub,
        stringsAsFactors = FALSE
      ))
    }
  }
}

df$white <- df$white - mean(df$white, na.rm = TRUE)
df$black <- df$black - mean(df$black, na.rm = TRUE)
df$asian <- df$asian - mean(df$asian, na.rm = TRUE)
df$aian <- df$aian - mean(df$aian, na.rm = TRUE)
# df$otherrace <- df$otherrace - mean(df$otherrace, na.rm = TRUE), excluding due to low counts
df$hispanic <- df$hispanic - mean(df$hispanic, na.rm = TRUE)

svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

results_income <- data.frame(outcome = character(), income_group = character(), year = integer(), 
                             rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                             stringsAsFactors = FALSE)

for (outcome in outcomes) {
  for (income_group in levels(df$income)) {
    temp <- subset(svy_design, income == income_group)
    temp$variables$year <- as.factor(temp$variables$year)
    formula <- paste(outcome, "~ age + sex + white + black + hispanic + asian + aian + neast + midwest + south + west + urbrur + year")
    
    fit <- svyglm(as.formula(formula), design = temp, family = quasibinomial(link = 'logit'))
    
    b <- coef(fit)
    V <- vcov(fit)
    draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
    coef_names <- names(b)
    coef_indices <- match(coef_names, colnames(draws))
    
    subset_draws <- draws[, coef_indices]
    
    for (year in 2019:2024) {
      n <- year - 2020 + 12 # since we are not including otherrace, we will not adjust for it
      
      if(year == 2019) {
        rates <- inv.logit(subset_draws[, 1])
      } else {
        rates <- inv.logit(subset_draws[, n] + subset_draws[, 1])
      }
      
      rate_mean <- mean(rates)
      rate_sd <- sd(rates)
      rate_lb <- quantile(rates, 0.025)
      rate_ub <- quantile(rates, 0.975)
      
      results_income <- rbind(results_income, data.frame(
        outcome = outcome, income_group = income_group, year = year, rate = rate_mean,
        stderr = rate_sd, lb = rate_lb, ub = rate_ub,
        stringsAsFactors = FALSE
      ))
    }
  }
}

write.csv(results_race, "health_access_race_results_111425_v1.csv", row.names = FALSE)
write.csv(results_income, "health_access_income_results_111425_v1.csv", row.names = FALSE)

outcome_labels <- c(
  "noinsurance" = "Uninsured, %",
  "badcarecost" = "Delayed or forwent medical\ncare due to cost in the past year, %",
  "crn_barrier" = "Experienced cost-related medication\nnonadherence in the past year, %",
  "noplace" = "Without a usual source of care, %"
)

make_panel <- function(data, group_var, outcome_name, show_legend, tag, legend_title) {
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
      limits = c(0, 0.25)
    ) +
    scale_x_continuous(breaks = 2019:2024) +
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

pA_race_fig1 <- make_panel(results_race, "race_group", "noinsurance", FALSE, "A", "Race/Ethnicity")
pB_race_fig1 <- make_panel(results_race, "race_group", "noplace", TRUE, "B", "Race/Ethnicity")
pC_race_fig1 <- make_panel(results_income, "income_group", "noinsurance", FALSE, "C", "Family Income")
pD_race_fig1 <- make_panel(results_income, "income_group", "noplace", TRUE, "D", "Family Income")

figure1 <- (pA_race_fig1 | pB_race_fig1) / (pC_race_fig1 | pD_race_fig1)

pA_race_fig2 <- make_panel(results_race, "race_group", "crn_barrier", FALSE, "A", "Race/Ethnicity")
pB_race_fig2 <- make_panel(results_race, "race_group", "badcarecost", TRUE, "B", "Race/Ethnicity")
pC_race_fig2 <- make_panel(results_income, "income_group", "crn_barrier", FALSE, "C", "Family Income")
pD_race_fig2 <- make_panel(results_income, "income_group", "badcarecost", TRUE, "D", "Family Income")

figure2 <- (pA_race_fig2 | pB_race_fig2) / (pC_race_fig2 | pD_race_fig2)

print(figure1)
print(figure2)

ggsave('fig2_access_race_income_111425_v1.pdf', figure1, device = pdf, width = 12, height = 8)
ggsave('fig2_cost_race_income_111425_v1.pdf', figure2, device = pdf, width = 12, height = 8)
