#### LIBRAIRIES ####
library(dplyr)
library(ggplot2)
library(data.table)
library(ExPanDaR)
library(panelr)
library(skimr)
library(stargazer)
library(DataExplorer)
library(plm)
library(lmtest)
library(kableExtra)
library(plotly)
library(pder)
#### FONCTIONS ####
source("R/fonctions.R")


#### DATA ####
df <- load_data()
rm("HousePricesUS")


#### STATISTIQUES ET EXPLORATION DES DONNEES ####
# Statistiques descriptives
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

# Density curve
density_curve(df, df$price)
density_curve(df, df$income)
density_curve(df, df$pop)
density_curve(df, df$intrate)

# Bar Chart
bar_chart(df, "names", "price", median, TRUE)
bar_chart(df, "names", "income", median, TRUE)
bar_chart(df, "names", "intrate", median, TRUE)


#### ESTIMATIONS WITHIN ####
# Création du data frame données de panel
panel_df <- pdata.frame(df, index = c("names", "year"), drop.index=TRUE, row.names=TRUE)

within_i <- panel_df_lm("within", "indiv") # individuel
within_t <- panel_df_lm("within", "time") # temporel
within <- panel_df_lm("within", "twoways") # total


#### ESTIMATIONS RANDOM EFFECTS ####
Fgls_i <- panel_df_lm("random", "indiv") # individuel
Fgls_t <- panel_df_lm("random", "time") # temporel
Fgls <- panel_df_lm("random", "twoways") # total


#### F-TESTS ####
pFtest(price ~ income + pop + intrate, data = panel_df, effect = "indiv") # individuel
pFtest(price ~ income + pop + intrate, data = panel_df, effect = "time") # temporel
pFtest(price ~ income + pop + intrate, data = panel_df, effect = "twoways") # total


#### LM-TESTS ####
lm_test("indiv")
lm_test("time")
lm_test("twoways")


#### HAUSMAN TEST ####
phtest(within_i, Fgls_i) # individuel
# -> ici on choisit random effects

phtest(within, Fgls) # twoways
# -> ici on choisit fixed effects (within)