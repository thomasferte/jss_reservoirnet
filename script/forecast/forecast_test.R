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
nb_iter = 40
##### LOAD BEST HP SETS
hp_sets = list(common_input_scaling = "data/common_input_scaling_11559350/",
               common_input_scaling_linked_source = "data/common_input_scaling_11559390/",
               multiple_input_scaling = "data/multiple_input_scaling_11559350/",
               multiple_input_scaling_linked_source = "data/multiple_input_scaling_11559390/",
               enet = "data/enet_1/") %>%
  lapply(function(path_i){
    list.files(path_i, full.names = TRUE) %>%
      lapply(readRDS) %>%
      bind_rows() %>%
      tibble::rowid_to_column(var = "hp_set")
  })

best_hp_set = lapply(hp_sets,
                     function(x) x %>% slice_min(mean_absolute_error, n = 1))
##### DEFINE EVALUATION SET
vecDates <- data_covid %>%
  filter(START_DATE >= as.Date("2021-03-01") + forecast_days) %>%
  pull(START_DATE)

date_i <- vecDates[slar_taskid]
message(paste0("--------- evaluation date : ", date_i, " ----------"))

##### FORECAST
dfres <- lapply(names(best_hp_set),
       function(model_set){
         print(model_set)
         hp_set <- best_hp_set[[model_set]]
         if(model_set == "enet"){
           ##### ELASTIC-NET
           forecast_enet_once <- fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                vecDates = date_i,
                                                                forecast_days = forecast_days,
                                                                lambda = best_hp_set$enet$lambda,
                                                                alpha = best_hp_set$enet$alpha,
                                                                model = "enet") %>%
             mutate(model = "enet_hp_on_train_set",
                    hp_set = hp_set$hp_set)
           forecast_enet_daily <- fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                                 vecDates = date_i,
                                                                 forecast_days = forecast_days,
                                                                 model = "enet") %>%
             mutate(model = "enet_daily_hp_update",
                    hp_set = 1)
           
           res <- bind_rows(forecast_enet_once, forecast_enet_daily)
         }
         
         if(model_set %in% c("common_input_scaling",
                             "common_input_scaling_linked_source",
                             "multiple_input_scaling",
                             "multiple_input_scaling_linked_source")){
           ##### RESERVOIR
           if(model_set %in% c("common_input_scaling", "common_input_scaling_linked_source")){
             input_scaling <- hp_set$input_scaling
           } else if(model_set %in% c("multiple_input_scaling", "multiple_input_scaling_linked_source")){
             input_scaling = hp_set %>%
               dplyr::select(-c("hp_set",
                                "ridge",
                                "leaking_rate",
                                "spectral_radius",
                                "seed",
                                "mean_absolute_error",
                                "time"))
           }
           res <- fct_iterative_forecast_from_hp(data_covid = data_covid,
                                                 vecDates = date_i,
                                                 warmup = warmup,
                                                 forecast_days = forecast_days,
                                                 units = units,
                                                 lr = hp_set$leaking_rate,
                                                 sr = hp_set$spectral_radius,
                                                 ridge = hp_set$ridge,
                                                 input_scaling = input_scaling,
                                                 link_source = hp_set$link_source,
                                                 model = "esn",
                                                 nb_iter = nb_iter) %>%
             mutate(hp_set = hp_set$hp_set,
                    model = model_set)
         }
         return(res)
       }) %>%
  bind_rows()

##### SAVE
message("--------- SAVE ----------")
fct_save_results(subDir = paste0("data/results_forecast_testset_", slar_jobid),
                 slar_taskid = slar_taskid,
                 object = dfres)

if(slar_taskid == 1){
  fct_save_results(subDir = paste0("data/results_forecast_testset_", slar_jobid),
                   slar_taskid = "hp_sets",
                   object = hp_sets)
}
