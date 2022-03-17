# IMPORT DATA -------------------------------------------------------------
load_data <- function() {
  data("HousePricesUS")
  as.data.frame(HousePricesUS)
  select(HousePricesUS,-c('state', 'region', 'region.name'))
}


# GRAPHIQUES TENDANCES ----------------------------------------------------
trend_plot <- function(data, ts, variable) {
  prepare_trend_graph(df = data, ts_id = ts, var = variable)
}


# DENSITY CURVE -----------------------------------------------------------
density_curve <- function(df, variable) {
  ggplot(data = df, aes(x = variable)) +
    geom_density(fill = "blue", alpha = .2) +
    stat_function(
      fun = dnorm,
      args = list(mean = mean(variable),
                  sd = sd(variable)),
      col = "Blue",
      size = 0.5
    )
}


# BAR CHART ---------------------------------------------------------------
bar_chart <- function(data,
                      sort_by,
                      variable,
                      stat,
                      order_by_stat) {
  prepare_by_group_bar_graph(
    df = data,
    by_var = sort_by,
    var = variable,
    stat_fun = stat,
    order_by_stat = order_by_stat
  )
}


# ESTIMATIONS PANEL -------------------------------------------------------
panel_df_lm <- function(model, effect) {
  plm(
    price ~ income + log(pop) + intrate,
    data = panel_df,
    model = model,
    effect = effect
  )
}


# LM-TEST -----------------------------------------------------------------
lm_test <- function(x) {
  plmtest(
    price ~ income + log(pop) + intrate,
    data = panel_df,
    effect = x,
    type = "bp"
  )
}














