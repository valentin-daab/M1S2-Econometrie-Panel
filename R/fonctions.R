# Importation des données
load_data <- function(){
  data("HousePricesUS")
  as.data.frame(HousePricesUS)
  # Retire les colonnes inutiles
  select(HousePricesUS, -c('state', 'region', 'region.name'))
}


# Graphique des tendances
trend_plot <- function(data, ts, variable){
  prepare_trend_graph(df = data, ts_id = ts, var = variable)
}


# Density Curve
density_curve <- function(df, variable){
  ggplot(data = df, aes(x = variable)) +
    geom_density(fill = "blue", alpha = .2) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(variable),
                              sd = sd(variable)),
                  col = "Blue",
                  size = 0.5)
}


# Bar Chart
bar_chart <- function(data, sort_by, variable, stat, order_by_stat){
  prepare_by_group_bar_graph(df = data,
                             by_var = sort_by,
                             var = variable,
                             stat_fun = stat,
                             order_by_stat = order_by_stat)
}


# Estimations panel
panel_df_lm <- function(model, effect){
  plm(price ~ income + pop + intrate,
      data = panel_df,
      model = model,
      effect = effect)
}


# LM Tests
lm_test <- function(x){
  plmtest(price ~ income + pop + intrate,
          data = panel_df,
          effect=x,
          type="bp")
}














