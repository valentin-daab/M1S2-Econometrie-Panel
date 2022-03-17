# LIBRAIRIES --------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(data.table)
library(ExPanDaR)
library(panelr)
library(skimr)
library(stargazer)
library(plm)
library(lmtest)
library(kableExtra)
library(plotly)
library(pder)
library(pander)



# FONCTIONS ---------------------------------------------------------------
source("R/fonctions.R")


# DATA --------------------------------------------------------------------
df <- load_data()
df <- rename(df, fips = plate)
rm("HousePricesUS")


# STATISTIQUES DESCRIPTIVES -----------------------------------------------
prepare_descriptive_table(df, format = "latex")
# Min Max Statistiques Descriptives
prepare_ext_obs_table(df, cs_id = "names", ts_id = "year", var = "pop")
# Trend Plot
trend_plot(df, "year", "price")
trend_plot(df, "year", "income")
trend_plot(df, "year", "pop")
trend_plot(df, "year", "intrate")
# Corr Plot
prepare_correlation_graph(df)
# Density Curve
density_curve(df, df$price)
density_curve(df, df$income)
density_curve(df, df$pop)
density_curve(df, df$intrate)
# Bar Chart
bar_chart(df, "names", "price", median, TRUE)
bar_chart(df, "names", "income", median, TRUE)
bar_chart(df, "names", "intrate", median, TRUE)


# PANEL DATA FRAME --------------------------------------------------------
panel_df <-
  pdata.frame(
    df,
    index = c("names", "year"),
    drop.index = TRUE,
    row.names = TRUE
  )


# POOLED OLS --------------------------------------------------------------
lm <- lm(price ~ income + log(pop) + intrate, data = panel_df)


# FIXED EFFECTS -----------------------------------------------------------
within_i <- panel_df_lm("within", "indiv") # individuel
within_t <- panel_df_lm("within", "time") # temporel
within <- panel_df_lm("within", "twoways") # total


# RANDOM EFFECTS ----------------------------------------------------------
Fgls_i <- panel_df_lm("random", "indiv") # individuel
Fgls_t <- panel_df_lm("random", "time") # temporel
Fgls <- panel_df_lm("random", "twoways") # total


# SUMMARY -----------------------------------------------------------------
stargazer(lm, within, Fgls)


# F-TEST ------------------------------------------------------------------
pftest_i <- pFtest(price ~ income + log(pop) + intrate, data = panel_df, effect = "indiv") # individuel
pftest_t <- pFtest(price ~ income + log(pop) + intrate, data = panel_df, effect = "time") # temporel
pftest <- pFtest(price ~ income + log(pop) + intrate, data = panel_df, effect = "twoways") # total


# LM-TEST -----------------------------------------------------------------
lmtest_i <- lm_test("indiv")
lmtest_t <- lm_test("time")
lmtest <- lm_test("twoways")


# SUMMARY TESTS -----------------------------------------------------------
pander(pftest_i)


# HAUSMAN TEST ------------------------------------------------------------
phtest_i <- phtest(within_i, Fgls_i)
phtest_t <- phtest(within_t, Fgls_t)
phtest <- phtest(within, Fgls)