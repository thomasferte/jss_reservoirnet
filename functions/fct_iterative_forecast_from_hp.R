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
#' @param link_source Should the input be linked to the target
#' @param model esn or enet
#' @param nb_iter Number of replication of the model
#' @param lambda The lambda elastic-net (use cv.glmnet if not supplied)
#' @param alpha The alpha parameter of elastic-net
#'
#' @return A dataframe with date, outcome date, forecast, outcome and current hospitalisations
fct_iterative_forecast_from_hp <- function(data_covid,
                                           vecDates,
                                           link_source = FALSE,
                                           model = "esn",
                                           warmup = 30,
                                           forecast_days = 14,
                                           units = 500,
                                           lr = 0.5,
                                           sr = 1,
                                           ridge = 1e2,
                                           input_scaling = 1,
                                           seed = 1,
                                           nb_iter = 1,
                                           lambda = NULL,
                                           alpha = 0.5){
  # iterate over each date in vecDates to train and forecast with a reservoir
  dfforecast <- lapply(vecDates,
                       function(date_i){
                         # prepare data
                         data_i <- fct_smoothing_derivative(data = data_covid,
                                                            maxOutcomeDate = date_i,
                                                            forecast_days = forecast_days)
                         # split train and test set
                         ls_train_test <- fct_train_test_sets(data_i)
                         
                         # iterate over nb_iter models
                         dfforecast <- lapply(seq_len(nb_iter),
                                              FUN = function(iter_i){
                                                if(model == "esn"){
                                                  seed <- ifelse(nb_iter > 1,
                                                                 yes = round(runif(n = 1,
                                                                                   min = 1,
                                                                                   max = 1e5)),
                                                                 no = seed)
                                                  print(seed)
                                                  # train esn on train set and predict on test set
                                                  forecast_deriv <- fct_fit_esn(
                                                    link_source = link_source,
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
                                                                                 Y = ls_train_test$Y,
                                                                                 lambda = lambda,
                                                                                 alpha = alpha)
                                                } else {
                                                  stop("unknown model argument")
                                                }
                                                # aggregate results and get last predicted value
                                                dfforecast <- fct_aggregate_forecast(data_i = data_i,
                                                                                     forecast_deriv = forecast_deriv)
                                              }) %>%
                           bind_rows(.id = "iter")
                         
                         return(dfforecast)
                       }) %>%
    bind_rows() %>%
    return()
}

