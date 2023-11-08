#' fct_iterative_forecast_from_hp
#' 
#' @description Perform iterative forecast depending on hyperparameters. Each day of vecDates, a new reservoir is trained and its forecast is stored.
#'
#' @param data_covid The data
#' @param vecDates The vector of dates, each date in this vector will be used to train and forecast.
#' @param warmup The reservoir warmup
#' @param forecast_days The forecast horizon
#' @param units The number of units in the reservoir
#' @param lr The leaking rate
#' @param sr The spectral radius
#' @param ridge The ridge penalty
#' @param input_scaling The input scaling
#' @param seed The seed setting the reservoir connection
#'
#' @return A dataframe with date, outcome date, forecast, outcome and current hospitalisations
fct_iterative_forecast_from_hp <- function(data_covid,
                                           vecDates,
                                           model = "esn",
                                           warmup = 30,
                                           forecast_days = 14,
                                           units = 500,
                                           lr = 0.5,
                                           sr = 1,
                                           ridge = 1e2,
                                           input_scaling = 1,
                                           seed = 1){
  # iterate over each date in vecDates to train and forecast with a reservoir
  dfforecast <- lapply(vecDates,
                       function(date_i){
                         # prepare data
                         data_i <- fct_smoothing_derivative(data = data_covid,
                                                            maxOutcomeDate = date_i,
                                                            forecast_days = forecast_days)
                         # split train and test set
                         ls_train_test <- fct_train_test_sets(data_i)
                         
                         if(model == "esn"){
                           # train esn on train set and predict on test set
                           forecast_deriv <- fct_fit_esn(
                             X = ls_train_test$X,
                             Xtest = ls_train_test$Xtest,
                             Y = ls_train_test$Y,
                             units = units,
                             warmup = warmup,
                             lr = lr,
                             sr = sr,
                             ridge = ridge,
                             input_scaling = input_scaling,
                             seed = seed
                           )
                         } else if(model == "enet"){
                           forecast_deriv <- fct_fit_enet(X = ls_train_test$X,
                                                          Xtest = ls_train_test$Xtest,
                                                          Y = ls_train_test$Y)
                         } else {
                           stop("unknown model argument")
                         }
                         # aggregate results and get last predicted value
                         dfforecast <- fct_aggregate_forecast(data_i = data_i,
                                                              forecast_deriv = forecast_deriv)
                         return(dfforecast)
                       }) %>%
    bind_rows() %>%
    return()
}

