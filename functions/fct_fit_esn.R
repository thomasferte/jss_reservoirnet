#' fct_fit_esn
#'
#' @description Function to fit reservoir depending on hyperparameters and forecast on Xtest
#'
#' @param X The X matrix
#' @param Y The outcome
#' @param units The number of nodes (default = 500)
#' @param warmup The warmup length
#' @param lr The leaking rate
#' @param sr The spectral radius
#' @param ridge The ridge penalty
#' @param input_scaling The input scaling
#' @param Xtest The test X matrix
#' @param seed The seed determining reservoir connections
#'
#' @import reservoirnet
#'
#' @return the reservoir forecast
#' @export
fct_fit_esn <- function(X,
                        Xtest,
                        Y,
                        units = 500,
                        warmup,
                        lr,
                        sr,
                        ridge,
                        input_scaling,
                        seed){
  ##### setup reservoir
  ## check if input scaling is a dataframe, if so, select the correct order of input scaling 
  if(is.data.frame(input_scaling)){
    input_scaling <- input_scaling %>%
      dplyr::select(colnames(X)) %>%
      as.numeric()
  }
  reservoir <-
    reservoirnet::createNode(
      nodeType = "Reservoir",
      units = as.integer(units),
      lr = lr,
      sr = sr,
      input_scaling = input_scaling,
      seed = seed
    )
  readout <- reservoirnet::createNode(nodeType = "Ridge",
                                      ridge = ridge)
  model <- reservoirnet::link(reservoir, readout)
  ##### fit reservoir
  fit <- reservoirnet::reservoirR_fit(node = model, X = X, Y = Y, warmup = warmup)
  ##### forecast
  res <- reservoirnet::predict_seq(node = fit$fit,
                                   X = Xtest,
                                   reset = TRUE)
  ##### return results
  return(as.numeric(res))
}
