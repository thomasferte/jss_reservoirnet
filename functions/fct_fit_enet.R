#' fct_fit_enet
#' 
#' @description Function to fit elastic-net regression
#'
#' @param X The training features
#' @param Xtest The test features
#' @param Y The training outcome
#'
#' @return A vector with elastic-net predictions
fct_fit_enet <- function(X, Xtest, Y){
  enet <- glmnet::cv.glmnet(x = X,
                            y = Y,
                            alpha = 0.5,
                            family = "gaussian")
  res <- predict(enet, newx = Xtest, s = "lambda.1se")
  return(as.numeric(res))
}
