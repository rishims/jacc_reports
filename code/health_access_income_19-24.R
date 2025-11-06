# Trends in Health Access, Affordability, and Utilization among US Adults by Income, 2019-2024
# Rishi M. Shah
# 04 November 2025

# import libraries
library(dplyr)
library(survey)
library(data.table)
library(ggsci)
library(ggpubr)
library(boot)

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

# analysis

# set survey design
options(survey.lonely.psu = "adjust")
svy_design <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                        weights = ~sampweight_pooled, data = df, nest = TRUE)

# define the outcomes
outcomes <- c("noinsurance", "badcarecost", "crn_barrier", "noplace", "novisit")

results <- data.frame(outcome = character(), income_group = character(), year = integer(), 
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
    coef_names <- names(b)
    coef_indices <- match(coef_names, colnames(draws))
    
    subset_draws <- draws[, coef_indices]
    
    for (year in 2019:2024) {
      n <- year - 2020 + 13 # check why this is 13 and not 14
      
      if(year == 2019)
      {
        rates <- inv.logit(subset_draws[, 1])
      }
      else
      {
        rates <- inv.logit(subset_draws[, n] + subset_draws[, 1])
      }
      
      rate_mean <- mean(rates)
      rate_sd <- sd(rates)
      rate_lb <- quantile(rates, 0.025)
      rate_ub <- quantile(rates, 0.975)
      
      results <- rbind(results, data.frame(outcome = outcome, income_group = income_group, year = year, rate = rate_mean,
                                           stderr = rate_sd, lb = rate_lb, ub = rate_ub,
                                           stringsAsFactors = FALSE))
    }
  }
}

# write.csv(results, file = "ratefile_accessibility_income_11525.csv", row.names = FALSE)

results <- results %>%
  mutate(
    year = as.numeric(year),
    outcome = factor(outcome,
                     levels = c("noinsurance", "badcarecost", "crn_barrier", "noplace", "novisit"),
                     labels = c("Uninsured", 
                                "Forgoing or delaying care due to cost in the past 12 mo.", 
                                "Experienced cost-related medication nonadherence in the past 12 mo.", 
                                "Without a usual source of care", 
                                "Not seen or talked to a health professional in the past 12 mo."))
  )

# add in telehealth panel
df_t <- subset(df, year != 2019)

df_t$telehealth_weights_pooled <- df_t$telehealth_weights
df_t$strata_pooled <- df_t$strata
df_t$psu_pooled <- df_t$psu

# set survey design
options(survey.lonely.psu = "adjust")
svy_design_t <- svydesign(ids = ~psu_pooled, strata = ~strata_pooled, 
                          weights = ~telehealth_weights_pooled, data = df_t, nest = TRUE)

results_t <- data.frame(outcome = character(), income_group = character(), year = integer(), 
                        rate = numeric(), stderr = numeric(), lb = numeric(), ub = numeric(),
                        stringsAsFactors = FALSE)

for (income_group in levels(df_t$income)) {
  temp <- subset(svy_design_t, income == income_group)
  temp$variables$year <- as.factor(temp$variables$year)
  
  fit <- svyglm(telehealth ~ age + sex + white + black + hispanic + asian + aian + otherrace + neast + midwest + south + west + urbrur + year, design = temp, family = quasibinomial(link = 'logit'))
  
  b <- coef(fit)
  V <- vcov(fit)
  draws <- MASS::mvrnorm(n = 10000, mu = b, Sigma = V)
  coef_names <- names(b)
  coef_indices <- match(coef_names, colnames(draws))
  
  subset_draws <- draws[, coef_indices]
  
  for (year in 2020:2024) {
    n <- year - 2021 + 13
    
    if(year == 2020)
    {
      rates <- inv.logit(subset_draws[, 1])
    }
    else
    {
      rates <- inv.logit(subset_draws[, n] + subset_draws[, 1])
    }
    
    rate_mean <- mean(rates)
    rate_sd <- sd(rates)
    rate_lb <- quantile(rates, 0.025)
    rate_ub <- quantile(rates, 0.975)
    
    results_t <- rbind(results_t, data.frame(outcome = "telehealth", income_group = income_group, year = year, rate = rate_mean,
                                             stderr = rate_sd, lb = rate_lb, ub = rate_ub,
                                             stringsAsFactors = FALSE))
  }
}

# write.csv(results_t, file = "ratefile_telehealth_income_11525.csv", row.names = FALSE)

results_t <- results_t %>%
  mutate(
    year = as.numeric(year),
    outcome = factor(outcome,
                     levels = c("telehealth"),
                     labels = c("Had a medical appointment by video or phone in the past 12 mo."))
  )

results_combined <- bind_rows(results, results_t)

results_combined <- results_combined %>%
  filter(!(outcome == "Had a medical appointment by video or phone in the past 12 mo." & year == 2019))

results_combined <- results_combined %>%
  mutate(
    y_min = 0,
    y_max = ifelse(outcome == "Had a medical appointment by video or phone in the past 12 mo.", 0.50, 0.25)
  )

fig2 <- ggplot(results_combined, aes(x = year, y = rate, color = income_group, shape = income_group, group = income_group)) +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  geom_line(size = 1) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.075, linewidth = 0.7) +
  scale_color_jama(name = "Income") +
  scale_shape_manual(
    name = "Income",
    values = c(16, 17, 15, 18, 8, 3)[seq_along(unique(results$income_group))]
  ) +
  facet_wrap(~ outcome, ncol = 2, scales = "free") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_pubr(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.title = element_text(face = "bold")
  ) +
  guides(
    color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 3)),
    shape = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 3))
  ) +
  labs(
    x = "Year",
    y = "Survey-Weighted National Prevalence (%)",
    title = "Trends in Health Care Access, Utilization, and Affordability by Income, 2019–2024"
  )

fig2 <- fig2 +
  geom_blank(data = results_combined %>%
               mutate(y_limit = ifelse(outcome == "Had a medical appointment by video or phone in the past 12 mo.", 0.50, 0)),
             aes(y = y_limit))

ggsave('fig2_accessibility_income_11525_v2.pdf', fig2, device = 'pdf', width = 15, height = 12)


