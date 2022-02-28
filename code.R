#### LIBRARIES ####

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


#### DATA ####
data("HousePricesUS")
df <- as.data.frame(HousePricesUS)

# Enlever les colonnes non pertinentes
df <- df %>% select(-c('state', 'plate', 'region', 'region.name'))

#### STATISTIQUES ET EXPLORATION DES DONNEES ####

# Valeurs manquantes
prepare_missing_values_graph(df, ts_id = "year")

# Statistiques descriptives
stats <- prepare_descriptive_table(df)
stats$kable_ret

# Min Max Statistiques Descriptives
stats <- prepare_ext_obs_table(df, cs_id = "names", ts_id = "year", var = "pop")
stats$df

# Trend Plot
graph <- prepare_trend_graph(df, ts_id = "year", 'price')
graph$plot

# Corr Plot
corr <- prepare_correlation_table(df, bold = 0.01, format="html")
corr$kable_ret


prepare_correlation_graph(df)

#density curve
ggplot(df, aes(x = income)) +
  geom_density(fill = "blue", alpha = .2) +
  xlim(5, 20)

# Scatter Plot
prepare_scatter_plot(df, x="year", y="price", color="names", loess = 1)

# Bar Chart
bar <- prepare_by_group_bar_graph(df, by_var = "names", var = "price",
                                  stat_fun = mean, order_by_stat = TRUE)
bar$plot

#### ESTIMATIONS WITHIN ####

panel_df <- pdata.frame(df, index = c("names", "year"), drop.index=TRUE, row.names=TRUE)

# within individuel
b_within_i <- plm(price ~ income + pop + intrate, data = panel_df, model="within", effect="indiv")
summary(b_within_i)

# within temporel
b_within_t <- plm(price ~ income + pop + intrate, data = panel_df, model="within", effect="time")
summary(b_within_t)

#within total
b_within <- plm(price ~ income + pop + intrate, data = panel_df, model="within", effect="twoways")
summary(b_within_t)

#### ESTIMATIONS RANDOM EFFECTS ####

# individuel
Fgls_i <- plm(price ~ income + pop + intrate, data = panel_df, model="random", effect="indiv")
summary(Fgls_i)

# temporel
Fgls_t <- update(Fgls_i, effect = 'time')
summary(Fgls_t)

# total
Fgls <- update(Fgls_i, effect = 'twoways')
summary(Fgls)

#### F-TESTS ####

# individuel
pFtest(price ~ income + pop + intrate, data = panel_df, effect="indiv")

# temporel
pFtest(price ~ income + pop + intrate, data = panel_df, effect="time")

# total
pFtest(price ~ income + pop + intrate, data = panel_df, effect="twoways")

#### HAUSMAN TEST ####

# individuel
pwi <- plm(price ~ income + pop + intrate, data = panel_df, model = 'within', effect = 'indiv')
pri <- plm(price ~ income + pop + intrate, data = panel_df, model = 'random', effect = 'indiv')
phtest(pwi,pri)
# -> ici on choisit random effects du coup

# twoways
pw <- plm(price ~ income + pop + intrate, data = panel_df, model = 'within', effect = 'twoways')
pr <- plm(price ~ income + pop + intrate, data = panel_df, model = 'random', effect = 'twoways')
phtest(pw,pr)
# -> ici on choisit fixed effects (within)
