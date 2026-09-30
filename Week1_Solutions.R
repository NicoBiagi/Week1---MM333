# clear environment
rm(list = ls())

# turn off scientific notation
options(scipen = 999)

# load library
library(tidyverse)
library(janitor)
library(psych)
library(effectsize)
library(broom)

# change wd
current_wd <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_wd)

# load csv 
customers <- read_csv("MM333_customer_marketing.csv", show_col_types = FALSE) |>
  clean_names()

# identify the variables
str(customers)

# inspect the data
customers |> glimpse()
customers |> names()
customers |> summary()
customers |> summarise(
  rows = n(),
  missing_values = sum(is.na(across(everything())))
)

# check the variable classes
customers |> summarise(across(everything(), class))

# check for duplicates
customers |>
  count(customer_id) |>
  filter(n > 1)

# calculate group-wise descriptive statistics
customers |>
  group_by(campaign) |>
  summarise(
    n = n(),
    mean_spend = mean(monthly_spend_gbp, na.rm = TRUE),
    sd_spend = sd(monthly_spend_gbp, na.rm = TRUE),
    median_spend = median(monthly_spend_gbp, na.rm = TRUE),
    min_spend = min(monthly_spend_gbp, na.rm = TRUE),
    max_spend = max(monthly_spend_gbp, na.rm = TRUE)
  )

# calculate group-wise descriptive statistics (using "psych" package)
customers |>
  dplyr::group_by(campaign) |>
  dplyr::group_modify(
    ~ psych::describe(.x$monthly_spend_gbp) |>
      tibble::as_tibble()
  )

# visualise the outcome
ggplot(customers, aes(x = campaign, y = monthly_spend_gbp, fill = campaign)) +
  geom_boxplot(alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.25, show.legend = FALSE) +
  labs(
    x = "Campaign exposure",
    y = "Monthly spending (£)",
    title = "Monthly spending by campaign exposure"
  ) +
  theme_minimal()

# inspect distribution
ggplot(customers, aes(x = monthly_spend_gbp)) +
  geom_histogram(bins = 20, colour = "white") +
  facet_wrap(~ campaign) +
  theme_minimal()

# q-q plots
ggplot(customers, aes(sample = monthly_spend_gbp)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ campaign) +
  theme_minimal()

# check variances
customers |>
  summarise(
    variance_campaign = var(monthly_spend_gbp[campaign == "Campaign"], na.rm = TRUE),
    variance_control = var(monthly_spend_gbp[campaign == "Control"], na.rm = TRUE)
  )

# Two Sample t-test
t_test_result <- t.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  var.equal = TRUE,
  alternative = "two.sided"
)
t_test_result

# Cohen's d for Two Sample t-test
effectsize::cohens_d(
  monthly_spend_gbp ~ campaign,
  data = customers,
  pooled_sd = TRUE
)

# tidy results of Two Sample t-test
broom::tidy(t_test_result)

# Welch's independent sample t-test
welch_t_result <- t.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  var.equal = FALSE,
  alternative = "two.sided"
)
welch_t_result

# Cohen's d for Welch's independent sample t-test
effectsize::cohens_d(
  monthly_spend_gbp ~ campaign,
  data = customers,
  pooled_sd = FALSE
)

# tidy results of Welch's independent sample t-test
broom::tidy(welch_t_result)

# Mann–Whitney U test
mann_whitney_result <- wilcox.test(
  monthly_spend_gbp ~ campaign,
  data = customers,
  exact = FALSE,
  alternative = "two.sided"
)
mann_whitney_result

# Effect size for Mann–Whitney U test
effectsize::rank_biserial(
  monthly_spend_gbp ~ campaign,
  data = customers
)

# tidy results of Mann–Whitney U test
broom::tidy(mann_whitney_result)

# fit a multiple linear regression model predicting monthly spending
model_1 <- lm(
  monthly_spend_gbp ~ campaign + website_visits + loyalty_status + age,
  data = customers
)
summary(model_1)

# create a tidy coefficient table
broom::tidy(model_1, conf.int = TRUE)

# regression diagnostics
par(mfrow = c(2, 2))
plot(model_1)
par(mfrow = c(1, 1))

# adding interaction between campaing and loyalty status to the model
model_2 <- lm(
  monthly_spend_gbp ~ campaign * loyalty_status + website_visits + age,
  data = customers
)
summary(model_2)
