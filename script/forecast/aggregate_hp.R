##### LOAD PACKAGES
library(dplyr)
library(ggplot2)
library(reservoirnet)
library(parallel)
##### LOAD FUNCTIONS
invisible(lapply(list.files(here::here("functions/"), full.names = TRUE), source))
##### LOAD DATA
data_covid <- readRDS(file = "data/df_obfuscated_epidemio.rds")
##### COMMON PARAMETERS
forecast_days = 14
warmup = 30
units = 500
nb_hp_set = 40
nb_cores = detectCores()-2
##### LOAD BEST HP SETS
best_hp_set = list(common_input_scaling = "data/common_input_scaling_11341624/",
                   multiple_input_scaling = "data/multiple_input_scaling_11341624/") %>%
  lapply(function(path_i){
    list.files(path_i, full.names = TRUE) %>%
      lapply(readRDS) %>%
      bind_rows() %>%
      tibble::rowid_to_column(var = "hp_set") %>%
      slice_min(mean_absolute_error, n = nb_hp_set)
  })

##### DEFINE EVALUATION SET
vecDates <- data_covid %>%
  filter(START_DATE >= as.Date("2021-03-01") + forecast_days) %>%
  pull(START_DATE)
##### COMMON INPUT SCALING
forecast_common_is <- mclapply(X = 1:nrow(best_hp_set$common_input_scaling),
                               mc.cores = nb_cores,	
                               FUN = function(row_i){
                                 row = best_hp_set$common_input_scaling[row_i,]
                                 fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                vecDates = vecDates,
                                                                warmup = warmup,
                                                                forecast_days = forecast_days,
                                                                units = units,
                                                                lr = as.numeric(row[["leaking_rate"]]),
                                                                sr = as.numeric(row[["spectral_radius"]]),
                                                                ridge = as.numeric(row[["ridge"]]),
                                                                input_scaling = as.numeric(row[["input_scaling"]]),
                                                                seed = as.integer(row[["seed"]])) %>%
                                   mutate(hp_set = row[["hp_set"]])
                               }) %>%
  bind_rows() %>%
  mutate(model = "esn_common_is")

##### MULTIPLE INPUT SCALING
# forecast_multiple_is <- mclapply(X = 1:nrow(best_hp_set$multiple_input_scaling),
forecast_multiple_is <- lapply(X = 1:nrow(best_hp_set$multiple_input_scaling),
                               # mc.cores = nb_cores,	
                               FUN = function(row_i){
                                 print(row_i)
                                 row = best_hp_set$multiple_input_scaling[row_i,]
                                 input_scaling = row %>%
                                   dplyr::select(-c("hp_set",
                                                    "ridge",
                                                    "leaking_rate",
                                                    "spectral_radius",
                                                    "seed",
                                                    "mean_absolute_error",
                                                    "time"))
                                 
                                 fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                vecDates = vecDates,
                                                                warmup = warmup,
                                                                forecast_days = forecast_days,
                                                                units = units,
                                                                lr = as.numeric(row[["leaking_rate"]]),
                                                                sr = as.numeric(row[["spectral_radius"]]),
                                                                ridge = as.numeric(row[["ridge"]]),
                                                                input_scaling = input_scaling,
                                                                seed = as.integer(row[["seed"]])) %>%
                                   mutate(hp_set = row[["hp_set"]])
                               }) %>%
  bind_rows() %>%
  mutate(model = "esn_multiple_is")

##### ELASTIC-NET
forecast_enet <- fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                vecDates = vecDates,
                                                forecast_days = forecast_days,
                                                model = "enet") %>%
  mutate(model = "enet",
         hp_set = 1)
##### SAVE
dfres = bind_rows(forecast_common_is,
                  forecast_multiple_is,
                  forecast_enet)
saveRDS(object = list(best_hp_set = best_hp_set,
                      dfres = dfres),
        file = "data/results_forecast_testset.rds")

dfres %>%
  group_by(model, outcomeDate) %>%
  reframe(forecast = median(forecast),
          outcome = unique(outcome)) %>%
  mutate(absolute_error = abs(forecast - outcome),
         relative_error = absolute_error/outcome) %>%
  group_by(model) %>%
  reframe(MAE = mean(absolute_error),
          MRE = median(relative_error, na.rm = TRUE))

dfres %>%
  group_by(model, outcomeDate, hosp) %>%
  summarise(forecast = median(forecast),
            outcome = unique(outcome),
            hosp = unique(hosp),
            .groups = "drop") %>%
  ggplot(mapping = aes(x = outcomeDate, y = forecast, color = model)) +
  geom_line() +
  geom_line(mapping = aes(y = outcome, color = "observed")) +
  geom_line(mapping = aes(y = hosp, color = "hospitalizations")) +
  scale_color_manual(values = c("#023047", "#219EBC", "#8ECAE6", "#FB8500", "#FFB703")) +
  theme_minimal()
