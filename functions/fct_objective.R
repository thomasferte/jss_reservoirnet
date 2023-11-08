fct_objective <- function(data_covid,
                          vecDates,
                          warmup = 30,
                          forecast_days = 14,
                          units = 500,
                          model = "enet",
                          lr = 0.5,
                          sr = 1,
                          ridge = 1e2,
                          input_scaling = 1,
                          seed = 1){
  # iterate over each date in vecDates to train and forecast with a reservoir
  fct_iterative_forecast_from_hp(data_covid = data_covid,
                                 vecDates = vecDates,
                                 model = model,
                                 warmup = warmup,
                                 forecast_days = forecast_days,
                                 units = units,
                                 lr = lr,
                                 sr = sr,
                                 ridge = ridge,
                                 input_scaling = input_scaling,
                                 seed = seed) %>%
    # get the MAE
    dplyr::mutate(absolute_error = abs(forecast - outcome)) %>%
    dplyr::pull(absolute_error) %>%
    mean() %>%
    return()
}

