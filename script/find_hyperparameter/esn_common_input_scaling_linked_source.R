##### LOAD PACKAGES
library(dplyr)
library(reservoirnet)
##### SLAR VARIABLES
slar_taskid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
slar_jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_JOB_ID"))
##### LOAD FUNCTIONS
invisible(lapply(list.files(here::here("functions/"), full.names = TRUE), source))
##### LOAD DATA (only data before 2021-03-01 for learning hp)
lsdataDates <- fct_load_data_find_hp()
##### SAMPLE HYPERPARAMETERS
n_samples = 20
dfHyperparam <- reservoirnet::random_search_hyperparam(n = n_samples,
                                                       ls_fct = list(ridge = function(n) reservoirnet::rloguniform(n = n, min = 1e-10, max = 1e5),
                                                                     input_scaling = function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5),
                                                                     spectral_radius = function(n) reservoirnet::rloguniform(n = n, min = 1e-5, max = 1e5),
                                                                     leaking_rate = function(n) reservoirnet::rloguniform(n = n, min = 1e-3, max = 1),
                                                                     seed = function(n) round(runif(n = n, min = 0, max = 1e3)))) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-search_id)
##### COMPUTE MEAN ABSOLUTE ERROR
dfres <- lapply(seq_len(n_samples),
       function(id_sample){
         time_start <- Sys.time()
         mean_absolute_error <- fct_objective(lsdataDates$data_covid,
                                              lsdataDates$vecDates,
                                              link_source = TRUE,
                                              warmup = 30,
                                              forecast_days = 14,
                                              units = 500,
                                              lr = dfHyperparam$leaking_rate[id_sample],
                                              sr = dfHyperparam$spectral_radius[id_sample],
                                              ridge = dfHyperparam$ridge[id_sample],
                                              input_scaling = dfHyperparam$input_scaling[id_sample],
                                              seed = dfHyperparam$seed[id_sample])
         time_end <- Sys.time()
         res <- dfHyperparam[id_sample,] %>%
           dplyr::mutate(mean_absolute_error = mean_absolute_error,
                         time = difftime(time_end, time_start, units = "secs") %>% as.numeric())
         return(res)
       }) %>%
  bind_rows()

##### SAVE RESULTS
fct_save_results(subDir = paste0("data/common_input_scaling_linked_source", slar_jobid),
                 slar_taskid = slar_taskid,
                 object = dfres)
