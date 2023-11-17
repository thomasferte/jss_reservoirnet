library(dplyr)
library(ggplot2)

ls_files_results <- list.files("data/results_forecast_testset_11463695/", full.names = TRUE)
ls_files_results_res <- grep(ls_files_results, pattern = "hp_sets", value = TRUE, invert = TRUE)
hp_file <- grep(ls_files_results, pattern = "hp_sets", value = TRUE)

##### hyperparameters plots
hp_features_reservoir <- c("input_scaling",
                           "spectral_radius",
                           "leaking_rate",
                           "ridge")
hp_features <- c("hosp",
                 "P_TOUS_AGES",
                 "P_60_90_PLUS_ANS",
                 "FRACP_TOUS_AGES",
                 "FRACP_60_90_PLUS_ANS",
                 "URG_covid_19_COUNT",
                 "IPTCC.mean",
                 "Vaccin_1dose",
                 "hosp_rolDeriv7",
                 "P_TOUS_AGES_rolDeriv7",
                 "P_60_90_PLUS_ANS_rolDeriv7",
                 "FRACP_TOUS_AGES_rolDeriv7",
                 "FRACP_60_90_PLUS_ANS_rolDeriv7",
                 "URG_covid_19_COUNT_rolDeriv7",
                 "IPTCC.mean_rolDeriv7",
                 "Vaccin_1dose_rolDeriv7")

feature_labels <- conservation_status <- c(
  "input_scaling" = "Input scaling",
  "leaking_rate" = "Leaking rate",
  "ridge" = "Ridge",
  "spectral_radius" = "Spectral radius",
  "hosp" = "Hospitalizations",
  "P_TOUS_AGES" = "Positive RT-PCR",
  "P_60_90_PLUS_ANS" = "Positive RT-PCR 60 yo+",
  "FRACP_TOUS_AGES" = "% Positive RT-PCR",
  "FRACP_60_90_PLUS_ANS" = "% Positive RT-PCR 60 yo+",
  "URG_covid_19_COUNT" = "Emergency sojourn with covid-19",
  "IPTCC.mean" = "IPTCC",
  "Vaccin_1dose" = "Vaccine",
  "hosp_rolDeriv7" = "Hospitalizations (1st deriv)",
  "P_TOUS_AGES_rolDeriv7" = "Positive RT-PCR (1st deriv)",
  "P_60_90_PLUS_ANS_rolDeriv7" = "Positive RT-PCR 60 yo+ (1st deriv)",
  "FRACP_TOUS_AGES_rolDeriv7" = "% Positive RT-PCR (1st deriv)",
  "FRACP_60_90_PLUS_ANS_rolDeriv7" = "% Positive RT-PCR 60 yo+ (1st deriv)",
  "URG_covid_19_COUNT_rolDeriv7" = "Emergency sojourn with covid-19 (1st deriv)",
  "IPTCC.mean_rolDeriv7" = "IPTCC (1st deriv)",
  "Vaccin_1dose_rolDeriv7" = "Vaccine (1st deriv)"
)

model_labels <- c(
  "common_input_scaling" = "Common IS \n\n R %>>% output",
  "common_input_scaling_linked_source" = "Common IS \n\n input + R %>>% output",
  "multiple_input_scaling" = "Multiple IS \n\n R %>>% output",
  "multiple_input_scaling_linked_source" = "Multiple IS \n\n input + R %>>% output"
)

df_hp <- readRDS(hp_file) %>%
  bind_rows(.id = "model") %>%
  group_by(model) %>%
  dplyr::mutate(rank_perf = dense_rank(mean_absolute_error),
                n_try = n(),
                rank_perf_color = case_when(rank_perf == 1 ~ "Best",
                                            rank_perf <= 10 ~ "Top 10",
                                            rank_perf <=  100 ~ "Top 100",
                                            rank_perf > 100 ~ "Others"),
                rank_perf_color = factor(rank_perf_color, levels = c("Best", "Top 10", "Top 100", "Others")),
                mean_absolute_error = if_else(mean_absolute_error >= 30, 30, mean_absolute_error)) %>%
  ungroup() %>%
  dplyr::select(all_of(c("mean_absolute_error",
                         "rank_perf_color",
                         "model",
                         hp_features_reservoir,
                         hp_features))) %>%
  tidyr::pivot_longer(cols = all_of(c(hp_features_reservoir, hp_features)), 
                      values_to = "HP_value", names_to = "HP") %>%
  na.omit() %>%
  filter(mean_absolute_error < 30)

plot_reservoir_hp <- df_hp %>%
  filter(HP %in% hp_features_reservoir) %>%
  ggplot(mapping = aes(x = HP_value, 
                       y = mean_absolute_error,
                       color = rank_perf_color)) +
  geom_point() +
  facet_grid(model ~ HP, scales = "free_x",
             labeller = labeller(model = model_labels,
                                 HP = feature_labels)) +
  scale_x_log10(labels = scales::trans_format("log10", scales::label_math(10^.x)),
                breaks = c(1e-10, 1e-5, 1e0, 1e5, 1e10),
                minor_breaks = 10^(seq(-10, 5))) +
  scale_color_manual(values = c("blue", "#E76F51", "#E9C46A", "#2A9D8F")) +
  lims(y = c(15, 30)) +
  theme_minimal() + 
  theme(strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(angle = 90)) +
  theme(legend.position = "bottom") +
  labs(x = "Hyperparameter value", 
       y = "Mean absolute error",
       color = "") 

plot_feature_input_scaling <- df_hp %>%
  filter(HP %in% hp_features) %>%
  ggplot(mapping = aes(x = HP_value, 
                       y = mean_absolute_error,
                       color = rank_perf_color)) +
  geom_point() +
  facet_grid(model ~ HP, scales = "free_x",
             labeller = labeller(model = model_labels,
                                 HP = feature_labels)) +
  scale_x_log10(labels = scales::trans_format("log10", scales::label_math(10^.x)),
                breaks = c(1e-10, 1e-5, 1e0, 1e5, 1e10),
                minor_breaks = 10^(seq(-10, 5))) +
  scale_color_manual(values = c("blue", "#E76F51", "#E9C46A", "#2A9D8F")) +
  lims(y = c(15, 30)) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        strip.text.y = element_text(angle = 0),
        strip.text.x = element_text(angle = 90)) +
  labs(x = "Hyperparameter value", 
       y = "Mean absolute error",
       color = "")

##### forecast plots
model_forecast_labels <- c(
  "esn_common_is" =   "Common IS \n\n R %>>% output",
  "esn_common_is_linked_source" =   "Common IS \n\n input + R %>>% output",
  "esn_multiple_is" =   "Multiple IS \n\n R %>>% output",
  "esn_multiple_is_linked_source" =   "Multiple IS \n\n input + R %>>% output",
  "enet" = "Elastic-net"
)

df_forecast <- lapply(ls_files_results_res, readRDS) %>%
  dplyr::bind_rows()

df_forecast_aggregated <- df_forecast %>%
  group_by(START_DATE, outcomeDate, model) %>%
  summarise(outcome = unique(outcome),
            forecast = median(forecast),
            hosp = unique(hosp),
            .groups = "drop") %>%
  dplyr::mutate(model = factor(model,
                               levels = names(model_forecast_labels),
                               labels = model_forecast_labels))

table_perf_esn <- df_forecast_aggregated %>%
  dplyr::mutate(absolute_error = abs(forecast - outcome),
                relative_error = absolute_error/outcome,
                baseline_absolute_error = abs(hosp - outcome),
                absolute_error_delta_baseline = absolute_error - baseline_absolute_error,
                absolute_error_relative_baseline = absolute_error/baseline_absolute_error) %>%
  group_by(model) %>%
  summarise(mae = mean(absolute_error),
            mre = median(relative_error, na.rm = TRUE),
            mae_baseline = mean(absolute_error_delta_baseline),
            mre_baseline = median(absolute_error_relative_baseline, na.rm = TRUE),
            .groups = "drop")

plot_forecast <- df_forecast_aggregated %>%
  tidyr::pivot_longer(cols = c(outcome, forecast, hosp)) %>%
  mutate(name = factor(name,
                       levels = c("outcome",
                                  "hosp",
                                  "forecast"),
                       labels = c("Outcome",
                                  "Baseline (smoothed hospitalizations)",
                                  "Model forecast"))) %>%
  ggplot(mapping = aes(x = outcomeDate, y = value, color = name)) +
  geom_line() +
  scale_color_manual(values = c("#023047", "#8ECAE6", "#FB8500")) +
  scale_x_date(date_breaks = "2 months", date_labels =  "%m-%y") +
  facet_wrap(model ~ ., ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Date (month-year)",
       y = "Hospitalizations",
       color = "")

##### aggregation forecast plots
nb_aggregation <- c(1, 5, 10, 20, 30, 40)
nb_boot <- 250

df_aggregation_forecast <- parallel::mclapply(seq_len(nb_boot),
                                              mc.cores = parallel::detectCores()-2, 
                                              function(boot_i){
                                                lapply(nb_aggregation,
                                                       function(nb_aggregation_i){
                                                         df_forecast %>%
                                                           filter(model != "enet") %>%
                                                           select(model, iter) %>%
                                                           distinct() %>%
                                                           group_by(model) %>%
                                                           sample_n(nb_aggregation_i, replace = TRUE) %>%
                                                           left_join(df_forecast,
                                                                     by = c("model", "iter"),
                                                                     relationship = "many-to-many") %>%
                                                           group_by(START_DATE, outcomeDate, model) %>%
                                                           summarise(outcome = unique(outcome),
                                                                     forecast = median(forecast),
                                                                     hosp = unique(hosp),
                                                                     .groups = "drop") %>%
                                                           dplyr::mutate(model = factor(model,
                                                                                        levels = names(model_forecast_labels),
                                                                                        labels = model_forecast_labels)) %>%
                                                           dplyr::mutate(absolute_error = abs(forecast - outcome),
                                                                         relative_error = absolute_error/outcome,
                                                                         baseline_absolute_error = abs(hosp - outcome),
                                                                         absolute_error_delta_baseline = absolute_error - baseline_absolute_error,
                                                                         absolute_error_relative_baseline = absolute_error/baseline_absolute_error) %>%
                                                           group_by(model) %>%
                                                           summarise(mae = mean(absolute_error),
                                                                     mre = median(relative_error, na.rm = TRUE),
                                                                     mae_baseline = mean(absolute_error_delta_baseline),
                                                                     mre_baseline = median(absolute_error_relative_baseline, na.rm = TRUE),
                                                                     .groups = "drop") %>%
                                                           mutate(nb_aggregation = nb_aggregation_i)
                                                       }) %>%
                                                  bind_rows() %>%
                                                  mutate(boot_i = boot_i)
                                              }) %>%
  bind_rows()

df_aggregation_forecast %>%
  group_by(model, nb_aggregation) %>%
  summarise(mae_mean = mean(mae),
            mae_inf = quantile(mae, 0.025),
            mae_sup = quantile(mae, 0.975)) %>%
  ggplot(mapping = aes(x = nb_aggregation,
                       y = mae_mean,
                       ymin = mae_inf,
                       ymax = mae_sup,
                       group = model)) +
  geom_ribbon() +
  facet_wrap(model ~ .) +
  lims(y = c(0,50))

df_forecast %>%
  ggplot(mapping = aes(x = outcomeDate, y = forecast)) +
  geom_point() +
  facet_wrap(model ~ ., scales = "free")
