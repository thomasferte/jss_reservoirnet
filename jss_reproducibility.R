## ---- setup, include=FALSE----------------------------------------------------------------------------------------------------
options(prompt = 'R> ', continue = '+ ')
set.seed(1)


## ----rcpresentation, echo=FALSE, fig.cap="my caption", out.width = '90%', fig.cap="Reservoir computing is composed of an input layer, a reservoir and an output layer. Connection between input layer and reservoir and inside reservoir are random. Only the output layer is optimized based on a ridge regression."----
# knitr::include_graphics("images/schema.png/image1.png")


## ----eval=FALSE---------------------------------------------------------------------------------------------------------------
## # Install reservoirnet package from CRAN
## install.packages("reservoirnet")


## ----message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------
# Load usefull packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(reservoirnet)
# load dfCovid data from the reservoirnet package which contains Covid data
data("dfCovid")
# Set the forecast horizon to 14 days
dist_forecast = 14
# Set the train-test split to 2022-01-01
traintest_date = as.Date("2022-01-01")


## -----------------------------------------------------------------------------------------------------------------------------
dfOutcome <- dfCovid %>%
  # outcome at 14 days
  mutate(outcome = lead(x = hosp, n = dist_forecast),
         # Create a new column 'outcome' which contains the number of hospitalizations ('hosp')
         # shifted forward by 'dist_forecast' days (14 days). This represents the outcome we want to predict.

         outcomeDate = date + dist_forecast,
         # Create a new column 'outcomeDate' which is the current date plus the forecast period (14 days).
         
         outcome_deriv = outcome - hosp) %>%
         # Create a new column 'outcome_deriv' which is the difference between the predicted outcome and current hospitalizations.
         # This represents the change in hospitalizations over the forecast period.

  # rolling average for tested and positive_pcr
  mutate_at(.vars = c("Positive", "Tested"),
            .funs = function(x) slider::slide_dbl(.x = x,
                                                  .before = 6,
                                                  .f = mean))
            # Apply a rolling mean (7-day average) to the 'Positive' and 'Tested' columns.
            # The 'slider::slide_dbl' function is used to calculate the mean over a window of 7 days (current day + 6 days before).
            # This smooths out daily fluctuations and provides a better trend indicator.



## ----covidintro, fig.cap="Hospitalizations, IPTCC and positive PCR of Bordeaux University Hospital.", echo = FALSE------------
# Create a figure with hospitalizations, positive and tested RT-PCR from covid data
dfOutcome %>%
  tidyr::pivot_longer(cols = c("hosp", "Positive", "Tested")) %>%
  ggplot2::ggplot(mapping = aes(x = date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  theme_minimal() +
  labs(color = "") +
  theme(legend.position = "bottom")


## -----------------------------------------------------------------------------------------------------------------------------
# Create a reservoir computing node using the 'createNode' function from the reservoirnet package.
# Arguments:
# - nodeType = "Reservoir": Specify the type of node to be a reservoir.
# - seed = 1: Set the seed for reproducibility, ensuring consistent results when the model is run multiple times.
# - units = 500: Set the number of reservoir units (neurons) to 500.
# - lr = 0.7: Set the leakage rate (lr) of the reservoir, which controls how quickly the reservoir state decays over time.
# - sr = 1: Set the spectral radius (sr) of the reservoir, which influences the stability and memory capacity of the reservoir.
# - input_scaling = 1: Set the input scaling factor, which scales the input signal before it is fed into the reservoir.

reservoir <- reservoirnet::createNode(nodeType = "Reservoir",
                                      seed = 1,
                                      units = 500,
                                      lr = 0.7,
                                      sr = 1,
                                      input_scaling = 1)


## -----------------------------------------------------------------------------------------------------------------------------
## select explanatory features of the train set and transform it to an array
X <- dfOutcome %>%
  filter(outcomeDate < traintest_date) %>%
  select(hosp, Positive, Tested) %>%
  as.matrix()


## -----------------------------------------------------------------------------------------------------------------------------
# Generate the state of the reservoir using the 'predict_seq' function from the reservoirnet package.
# Arguments:
# - node = reservoir: The reservoir computing node created earlier.
# - X = X: The input data matrix containing the features 'hosp', 'Positive', and 'Tested'.
# The function computes the state of the reservoir for each time step in the input sequence,
# effectively transforming the input data into the reservoir's high-dimensional state space.

reservoir_state <- predict_seq(node = reservoir, X = X)


## ----nodeactivationbad, fig.cap="20 random nodes activation over time."-------------------------------------------------------
# Plot the reservoir state activation over time
plot(reservoir_state)


## -----------------------------------------------------------------------------------------------------------------------------
# Standardise features by dividing by the maximum value can improve performance
# After standardisation, all features are on a similar scale which helps RC
stand_max <- function(x) return(x/max(abs(x)))
# scaled features
Xstand <- dfOutcome %>%
  filter(date < traintest_date) %>%
  select(hosp, Positive, Tested) %>%
  mutate_all(.funs = stand_max) %>%
  as.matrix() %>%
  as.array()


## ----nodeactivationgood, fig.cap="20 random node activation over time. Scaled features."--------------------------------------
# feed the scaled features to the reservoir
reservoir_state_stand <- predict_seq(node = reservoir,
                                     X = Xstand,
                                     reset = TRUE)
# plot the output
plot(reservoir_state_stand)


## -----------------------------------------------------------------------------------------------------------------------------
readout <- reservoirnet::createNode(nodeType = "Ridge",
                                    ridge = 1e3)
# Create a readout node using ridge regression with the 'createNode' function from the reservoirnet package.
# Arguments:
# - nodeType = "Ridge": Specify the type of node to be a ridge regression readout.
# - ridge = 1e3: Set the regularization parameter (ridge) for the ridge regression to 1000.
# Ridge regression is used to prevent overfitting by adding a penalty on the size of the coefficients.

model <- reservoirnet::link(reservoir, readout)
# Link the reservoir and readout nodes to form a complete reservoir computing model.
# The 'link' function connects the high-dimensional state generated by the reservoir to the readout layer,
# allowing the model to learn the mapping from the reservoir states to the target outputs.


## -----------------------------------------------------------------------------------------------------------------------------
# Perform some data management to isolate train and test sets
# train set
dftrain <- dfOutcome %>% filter(outcomeDate <= traintest_date)
yTrain <- dftrain %>% select(outcome)
yTrain_variation <- dftrain %>% select(outcome_deriv)
xTrain <- dftrain %>% select(hosp, Positive, Tested)
# test set
xTest <- dfOutcome %>% select(hosp, Positive, Tested)


## ----results=FALSE------------------------------------------------------------------------------------------------------------
# copy train and test sets
xTrainstand <- xTrain
xTeststand <- xTest
# standardise based on training set values
ls_fct_stand <- apply(xTrain,
                      MARGIN = 2,
                      FUN = function(x) function(feature) return(feature/(max(x))))
lapply(X = names(ls_fct_stand),
       FUN = function(x){
         xTrainstand[,x] <<- ls_fct_stand[[x]](feature = xTrain[,x])
         xTeststand[,x] <<- ls_fct_stand[[x]](feature = xTest[,x])
         return()
       })
# convert to array
lsdf <- lapply(list(yTrain = yTrain,
                    yTrain_variation = yTrain_variation,
                    xTrain = xTrainstand,
                    xTest = xTeststand),
               function(x) as.matrix(x))


## -----------------------------------------------------------------------------------------------------------------------------
### train the reservoir ridge output
fit <- reservoirnet::reservoirR_fit(node = model,
                                    X = lsdf$xTrain,
                                    Y = lsdf$yTrain,
                                    warmup = 30,
                                    reset = TRUE)


## -----------------------------------------------------------------------------------------------------------------------------
# Forecast with the trained reservoir on the test data
vec_pred <- reservoirnet::predict_seq(node = fit$fit,
                                      X = lsdf$xTest,
                                      reset = TRUE)


## ----fig.cap="Forecast", fig.height=4, fig.width=7----------------------------------------------------------------------------
# Make figure to represent forecast on the train and test sets.

dfOutcome %>%
  mutate(pred = vec_pred) %>%
  na.omit() %>%
  ggplot(mapping = aes(x = outcomeDate)) +
  geom_line(mapping = aes(y = outcome,
                          color = "observed")) +
  geom_line(mapping = aes(y = pred,
                          color = "forecast")) +
  annotate("rect",
           xmin = traintest_date,
           xmax = max(dfOutcome$outcomeDate, na.rm = T),
           ymin = 0,
           ymax = max(dfOutcome$outcome, na.rm = T)*1.1, 
           alpha = .2) +
  annotate("text", label = "Test set", x = as.Date("2022-08-01"), y = 2200, size = 7) +
  annotate("text", label = "Train set", x = as.Date("2021-03-01"), y = 2200, size = 7) +
  scale_color_manual(values = c("#3772ff", "#080708")) +
  theme_minimal() +
  labs(color = "", x = "Date", y = "Hospitalizations")


## ----figForecastDerivvsRaw, fig.cap="Covid-19 hospitalizations forecast. The model is either trained to forecast the number of hospitalizations (denoted Raw) or the variation of the hospitalizations compared to current level of hospitalisation (denoted Variation)", fig.height=4, fig.width=7----
## Fit reservoir on outcome variation instead of raw outcome
fit2 <- reservoirnet::reservoirR_fit(node = model,
                                     X = lsdf$xTrain,
                                     Y = lsdf$yTrain_variation,
                                     warmup = 30,
                                     reset = TRUE)
## Get the forecast on the test set
vec_pred2_variation <- reservoirnet::predict_seq(node = fit2$fit,
                                                 X = lsdf$xTest,
                                                 reset = TRUE)
## Transform the outcome variation forecast into hospitalization forecast
vec_pred2 <- vec_pred2_variation + xTest$hosp
## Plot the results
dfOutcome %>%
  mutate(Raw = vec_pred,
         Variation = vec_pred2) %>%
  tidyr::pivot_longer(cols = c(Raw, Variation),
                      names_to = "Outcome_type",
                      values_to = "Forecast") %>%
  na.omit() %>%
  ggplot(mapping = aes(x = outcomeDate)) +
  geom_line(mapping = aes(y = outcome,
                          color = "observed")) +
  geom_line(mapping = aes(y = Forecast,
                          color = "Forecast")) +
  annotate("rect",
           xmin = traintest_date,
           xmax = max(dfOutcome$outcomeDate, na.rm = T),
           ymin = 0,
           ymax = max(dfOutcome$outcome, na.rm = T)*1.1, 
           alpha = .2) +
  annotate("text", label = "Test set", x = as.Date("2022-08-01"), y = 2200, size = 5) +
  annotate("text", label = "Train set", x = as.Date("2021-03-01"), y = 2200, size = 5) +
  facet_wrap(Outcome_type ~ .,
             labeller = label_bquote(cols = "Outcome" : .(Outcome_type))) +
  scale_color_manual(values = c("#3772ff", "#080708")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(color = "", x = "Date", y = "Hospitalizations")


## -----------------------------------------------------------------------------------------------------------------------------
# Get the Japanese vowels dataset using the 'generate_data' function from the reservoirnet package.
# The dataset contains preprocessed features and labels for classification.
# Then we isolate train and test sets
japanese_vowels <- reservoirnet::generate_data(dataset = "japanese_vowels")[[1]]
X_train <- japanese_vowels$X_train
Y_train <- japanese_vowels$Y_train
X_test <- japanese_vowels$X_test
Y_test <- japanese_vowels$Y_test


## ----vowelpresentation, fig.cap="Vowel dataset, sample with 3 speakers and 2 utterance each.", echo = FALSE-------------------
# Make a figure of the Vowel dataset of selected participants.
vec_sample <- c(1, 2, 41, 42, 71, 72)
dfplot_vowel <- lapply(vec_sample,
                 FUN = function(i){
                   speaker <- which(Y_test[[i]] == 1)
                   X_test[[i]] %>%
                     as.data.frame() %>%
                     tibble::rowid_to_column(var = "Time") %>%
                     tidyr::pivot_longer(cols = -Time,
                                         names_to = "component",
                                         values_to = "LPC") %>%
                     mutate(speaker = speaker, .before = 1,
                            uterrance = i) %>%
                     return()
                 }) %>%
  bind_rows()

ggplot(dfplot_vowel, mapping = aes(x = Time, y = LPC, color = component)) +
  geom_line() +
  facet_wrap(uterrance ~ speaker,
             labeller = label_bquote(cols = "speaker" : .(speaker)),
             ncol = 2) +
  theme_minimal() +
  theme(legend.position = "none")



## -----------------------------------------------------------------------------------------------------------------------------
reservoir <- reservoirnet::createNode("Reservoir", units = 500,
                                      lr=0.1, sr=0.9,
                                      seed = 1)
# Create a reservoir computing node with 500 units using the 'createNode' function from the reservoirnet package.
# Arguments:
# - units = 500: Set the number of reservoir units (neurons) to 500.
# - lr = 0.1: Set the leakage rate (lr) of the reservoir to 0.1, controlling how quickly the reservoir state decays over time.
# - sr = 0.9: Set the spectral radius (sr) of the reservoir to 0.9, influencing the stability and memory capacity of the reservoir.
# - seed = 1: Set the seed for reproducibility, ensuring consistent results when the model is run multiple times.
readout <- reservoirnet::createNode("Ridge",ridge=1e-6)
# Create a readout node using ridge regression with the 'createNode' function from the reservoirnet package.
# Arguments:
# - ridge = 1e-6: Set the regularization parameter (ridge) for the ridge regression to 1e-6.
# Ridge regression is used to prevent overfitting by adding a penalty on the size of the coefficients.


## -----------------------------------------------------------------------------------------------------------------------------
states_train = list()
k <- 1
for (x in X_train) {
  # Loop over each training sample in X_train.
  states <- reservoirnet::predict_seq(node = reservoir, X = x,
                                      reset=TRUE)
  # Generate the reservoir states for the current training sample using the 'predict_seq' function.
  states_train[[k]] <- t(as.matrix(states[nrow(states),]))
  # Extract the final reservoir state for the current sample and store it in 'states_train'.
  k <- k+1
}


## -----------------------------------------------------------------------------------------------------------------------------
# Fit the reservoir using the last state vector (each observation is the whole vowel sequence)
res <- reservoirnet::reservoirR_fit(readout,X = states_train, Y = Y_train)


## -----------------------------------------------------------------------------------------------------------------------------
# The operation is repeated for the test set :
# - the last state vector is extracted for each utterance
# - the model is used to forecast the utterance from this last state vector
Y_pred <- list()
k <- 1
for (x in X_test) {
  states <- reservoirnet::predict_seq(node = reservoir, X = x,
                                      reset=TRUE)
  y <- reservoirnet::predict_seq(node = readout,
                                 X = as.array(states[nrow(states),]))
  Y_pred[[k]] <- y
  k <- k+1
}


## ----seqtovec, fig.cap="Prediction in a sequence-to-sequence approach 6 samples with 3 speakers and 2 utterance each. The speaker to predict is depicted in blue. For each of the 6 utterance, the model correctly identifies the speaker."----
# A figure represents the performance on the test set
dfplotseqtovec <- lapply(vec_sample,
                 FUN = function(i){
                   speaker <- which(Y_test[[i]][1,] == 1)
                   Y_pred[[i]] %>%
                     as.data.frame() %>%
                     tidyr::pivot_longer(cols = everything(),
                                         names_to = "pred_speaker",
                                         values_to = "prediction") %>%
                     mutate(pred_speaker = gsub(x = pred_speaker,
                                                pattern = "V", "")) %>%
                     mutate(speaker = speaker, .before = 1,
                            uterrance = i,
                            target = speaker == pred_speaker) %>%
                     return()
                 }) %>%
  bind_rows()

ggplot(dfplotseqtovec,
       mapping = aes(x = pred_speaker,
                     y = prediction,
                     fill = target)) +
  geom_bar(stat = "identity") +
  facet_wrap(uterrance ~ speaker,
             labeller = label_bquote(cols = "speaker" : .(speaker)),
             ncol = 2) +
  scale_fill_manual(values = c("#BDBDBD", "#A3CEF1")) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(y = 'Score',
       x = "Speaker")


## -----------------------------------------------------------------------------------------------------------------------------
# The overall accuracy is evaluated
accuracy <- function(pred, truth) mean(pred == truth)

Y_pred_class <- sapply(Y_pred,
                       FUN = function(x) apply(as.matrix(x),1,which.max))
Y_test_class <- sapply(Y_test,
                       FUN = function(x) apply(as.matrix(x),1,which.max))

score <- accuracy(pred = Y_test_class, truth = Y_pred_class)

print(paste0("Accuracy: ", round(score * 100,3) ,"%"))


## -----------------------------------------------------------------------------------------------------------------------------
# For this new task where we want to forecast for each time step (instead of each utterance)
# we start by getting the data in the appropriate format
# Then we split the train and test data
japanese_vowels <- reservoirnet::generate_data(
    dataset = "japanese_vowels",
    repeat_targets=TRUE)$japanese_vowels
X_train <- japanese_vowels$X_train
Y_train <- japanese_vowels$Y_train
X_test <- japanese_vowels$X_test
Y_test <- japanese_vowels$Y_test


## -----------------------------------------------------------------------------------------------------------------------------
# Create an input, a reservoir and an output layers
source <- createNode("Input")
readout <- createNode("Ridge",ridge=1e-6)
reservoir <- createNode("Reservoir",units = 500,lr=0.1, sr=0.9, seed = 1)
# Connect the input layer to the reservoir and connect both the input layer and the reservoir to the output layer
model <- list(source %>>% reservoir, source) %>>% readout


## -----------------------------------------------------------------------------------------------------------------------------
# Fit the RC model
model_fit <- reservoirnet::reservoirR_fit(node = model,
                                          X = X_train,
                                          Y = Y_train,
                                          warmup = 2)
# Predict with the fitted model
Y_pred <- reservoirnet::predict_seq(node = model_fit$fit,
                                    X = X_test,
                                    reset = TRUE)


## ----figseqtoseq, fig.cap="Prediction in a sequence-to-sequence approach 6 samples with 3 speakers and 2 utterance each. The higher the score of the speaker, the lighter the color.", fig.height=6----
# Make a graph with a label for each time of each utterance
dfplotseqtoseq <- lapply(vec_sample,
                 FUN = function(i){
                   speaker <- which(Y_test[[i]][1,] == 1)
                   Y_pred[[i]] %>%
                     as.data.frame() %>%
                     tibble::rowid_to_column(var = "Time") %>%
                     tidyr::pivot_longer(cols = -Time,
                                         names_to = "pred_speaker",
                                         values_to = "prediction") %>%
                     mutate(pred_speaker = gsub(x = pred_speaker,
                                                pattern = "V", ""),
                            speaker = speaker,
                            uterrance = i,
                            .before = 1) %>%
                     return()
                 }) %>%
  bind_rows()

ggplot(dfplotseqtoseq, mapping = aes(x = Time,
                                     y = pred_speaker,
                                     fill = prediction)) +
  geom_tile() +
  facet_wrap(uterrance ~ speaker,
             labeller = label_bquote(cols = "speaker" : .(speaker)),
             ncol = 2) +
  scale_fill_gradient2(low = "#8ECAE6", high = "#FB8500", mid = "#023047",
                       midpoint = 0) +
  theme_minimal() +
  labs(y = 'Predicted speaker',
       fill = "Prediction score")


## -----------------------------------------------------------------------------------------------------------------------------
# Compute the accuracy
Y_pred_class <- sapply(Y_pred, FUN = function(x) apply(as.matrix(x),
                                                       1,
                                                       which.max))
Y_test_class <- sapply(Y_test, FUN = function(x) apply(as.matrix(x),
                                                       1,
                                                       which.max))
score <- accuracy(array(unlist(Y_pred_class)), array(unlist(Y_test_class)))

print(paste0("Accuracy: ", round(score * 100,3) ,"%"))


## ----echo=FALSE---------------------------------------------------------------------------------------------------------------
# because computing time for this section is long, all results were computed
# using a calculation server and stored into a rds file
bool_precompute <- file.exists(here::here("data/precomputed_results.rds"))

if(bool_precompute){
  ls_use_case <- readRDS(file = here::here("data/precomputed_results.rds"))
  
  
  ## ----ucPresentData, echo = FALSE, fig.cap = "Covid-19 epidemic at BUH. Outcome of interest is presented in orange. Model is trained to forecast Outcome curve which corresonds to the difference between Hospitalisatiosn at 14 days and current hospitalisations. Other features are scaled (divide by the maximum of the feature) represented in darkblue.", fig.height=8----
  # descriptive plot of the data
  ls_use_case$plot_present_data
  
  
  ## ----ucHyperparam, echo=FALSE, fig.cap="Hyperparameter evaluation on training set by random search. Hp sets with MAE above 30 were removed for clarity of visualisation.", fig.height=8----
  # hyperparameter search
  ls_use_case$plot_reservoir_hp
  
  
  ## ----ucHyperparamMultipleIS, echo=FALSE, fig.cap="Hyperparameter evaluation on training set by random search of the model with multiple input scaling and no connection between input layer and output layer. Hp sets with MAE above 30 were removed for clarity of visualisation.", fig.height=8----
  # hyperparameter search, focus on input scaling
  ls_use_case$plot_feature_input_scaling
  
  
  ## ----performance, echo = FALSE------------------------------------------------------------------------------------------------
  # performance of the different model
  ls_use_case$table_perf_esn %>%
    knitr::kable(caption = "Model performance with several reservoir configuration. For each setting, 40 reservoir are computed and the forecast is the median of the 40 forecast. Results show the performance metrics : MAE = Mean Absolute Error, MRE = Median Relative Error, MAEB = Mean Absolute Error to Baseline, MREB = Median Relative Error to Baseline.",
                 col.names = c("Model", "MAE", "MRE", "MAEB", "MREB"),
                 booktabs = TRUE,
                 linesep = c('', '\\addlinespace'), digits = 2)
  
  
  ## ----forecastwithorwithoutUpdate, echo = FALSE, fig.cap = "Reservoir computing forecast depending on the setting with and without monthly update. Red line is the median forecast of 40 reservoirs. Grey lines are individual forecast of each of the 40 reservoir.", fig.height=8----
  # plot of the forecast of the different models
  ls_use_case$plot_forecast
  
  
  ## ----ucAggregationSpread, echo = FALSE, fig.cap = "Individual forecast the 40 best hyperparameter sets for the different RC configuration. Forecast value above 200 were set to 200 for clarity.", fig.height=6----
  # plot of different models without forecast aggregation.
  ls_use_case$plot_before_aggregation_forecast
  
  
  ## ----ucAggregationNb, echo = FALSE, fig.cap = "Mean absolute error depending on the number of aggregated reservoir.", fig.height=6----
  # plot models depending on number of aggregated reservoir.
  ls_use_case$plot_aggregation
  
  
  ## ----importanceReservoir, fig.cap="Mean feature importance of the 40 best hyperparameter sets by model, focus on the connection between the input and output layers. Models with direct connection between input and output layer are included. The rank is obtained by comparing the feature input layer and all other connection coefficients (both input and reservoir corresponding coefficients) attributed by the output layer at each date for each hyperparameter set. The higher the output layer's coefficient for the input layer, the closer its rank will be to 1 and the more important the feature is.", fig.height=6----
  # Plot raw feature importance
  ls_use_case$plot_reservoir_importance
  
  
  ## ----importanceReservoirvsEnet, fig.cap="Mean feature coefficient of the 40 best hyperparameter sets by model and the elastic-net model. Only models with direct connection between input and output layer are included. The coefficients were calculated as the average value across all dates for each feature, model and hyperparamet set.", fig.height=6----
  # Plot feature importance of different models
  ls_use_case$plot_importance
}
