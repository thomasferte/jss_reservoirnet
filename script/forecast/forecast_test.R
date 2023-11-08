##### LOAD PACKAGES
library(dplyr)
library(reservoirnet)
##### SLAR VARIABLES
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
slar_jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_JOB_ID"))
##### LOAD FUNCTIONS
invisible(lapply(list.files(here::here("functions/"), full.names = TRUE), source))
##### LOAD DATA
data_covid <- readRDS(file = "data/df_obfuscated_epidemio.rds")
##### COMMON PARAMETERS
forecast_days = 14
warmup = 30
units = 500
nb_hp_set = 40
##### LOAD BEST HP SETS
best_hp_set = list(common_input_scaling = "data/common_input_scaling_11374318/",
                   common_input_scaling_linked_source = "data/common_input_scaling_linked_source11374319/",
                   multiple_input_scaling = "data/multiple_input_scaling_11374318/",
                   multiple_input_scaling_linked_source = "data/multiple_input_scaling_linked_source11374319/") %>%
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

date_i <- vecDates[slar_taskid]
message(paste0("--------- evaluation date : ", date_i, " ----------"))
##### COMMON INPUT SCALING
message("--------- COMMON INPUT SCALING ----------")
forecast_common_is <- lapply(X = 1:nrow(best_hp_set$common_input_scaling),
                             FUN = function(row_i){
                               row = best_hp_set$common_input_scaling[row_i,]
                               fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                              vecDates = date_i,
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
##### COMMON INPUT SCALING (linked source)
message("--------- COMMON INPUT SCALING LS ----------")
forecast_common_is_linked_source <- lapply(X = 1:nrow(best_hp_set$common_input_scaling_linked_source),
                                           FUN = function(row_i){
                                             row = best_hp_set$common_input_scaling_linked_source[row_i,]
                                             fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                            vecDates = date_i,
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
  mutate(model = "esn_common_is_linked_source")
##### MULTIPLE INPUT SCALING
message("--------- MULTIPLE INPUT SCALING ----------")
forecast_multiple_is <- lapply(X = 1:nrow(best_hp_set$multiple_input_scaling),
                               FUN = function(row_i){
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
                                                                vecDates = date_i,
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
##### MULTIPLE INPUT SCALING  (linked source)
message("--------- MULTIPLE INPUT SCALING LS ----------")
forecast_multiple_is_linked_source <- lapply(X = 1:nrow(best_hp_set$multiple_input_scaling_linked_source),
                                             FUN = function(row_i){
                                               row = best_hp_set$multiple_input_scaling_linked_source[row_i,]
                                               input_scaling = row %>%
                                                 dplyr::select(-c("hp_set",
                                                                  "ridge",
                                                                  "leaking_rate",
                                                                  "spectral_radius",
                                                                  "seed",
                                                                  "mean_absolute_error",
                                                                  "time"))
                                               
                                               fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                              vecDates = date_i,
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
  mutate(model = "esn_multiple_is_linked_source")
##### ELASTIC-NET
message("--------- ELASTIC-NET ----------")
forecast_enet <- fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                vecDates = date_i,
                                                forecast_days = forecast_days,
                                                model = "enet") %>%
  mutate(model = "enet",
         hp_set = 1)
##### SAVE
message("--------- SAVE ----------")
dfres = bind_rows(forecast_common_is,
                  forecast_common_is_linked_source,
                  forecast_multiple_is,
                  forecast_multiple_is_linked_source,
                  forecast_enet)

fct_save_results(subDir = paste0("data/results_forecast_testset_", slar_jobid),
                 slar_taskid = slar_taskid,
                 object = dfres)