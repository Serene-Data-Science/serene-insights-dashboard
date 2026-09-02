##### Packages #####
library(mixtape)
library(dplyr)
library(tidyr)
library(arrow)

##### Config #####
source('config-files/config-project.R')

#-- dashboard is scoped to this client only for now
v_client_name <- 'data-one'

v_segment_types_sql_list <- "'overall', 'age_band', 'salary_band', 'derived_gender', 'imd_dec'"

#-- floor applied to every query below, so the app never sees data older than this
v_min_month <- '2019-01'

##### Data Reads #####

#----- SereneScore

ds_id_summary <- mix_databricks_read(
  query = paste0(
    "SELECT * FROM serene_data_science.05_report_layer.id_summary ",
    "WHERE client_name = '", v_client_name, "' AND segment_type IN (", v_segment_types_sql_list, ") ",
    "AND month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

#-- risk band mix chart is national (unsegmented), so only the 'overall' slice is needed
ds_id_risk_bands <- mix_databricks_read(
  query = paste0(
    "SELECT * FROM serene_data_science.05_report_layer.id_risk_bands ",
    "WHERE client_name = '", v_client_name, "' AND segment_type = 'overall' ",
    "AND month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

#-- top Serene ID table is national (unsegmented), so only the 'overall' slice is needed
ds_id_distribution <- mix_databricks_read(
  query = paste0(
    "SELECT * FROM serene_data_science.05_report_layer.id_distribution ",
    "WHERE client_name = '", v_client_name, "' AND segment_type = 'overall' ",
    "AND month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

#-- KPI value boxes need "how many customers we actually have," not whichever month happens to be
#-- the latest across the whole id_summary table -- a client's per-user data can lag, so that latest
#-- month can be sparsely populated and understate the real customer base. id_summary_snapshot is one
#-- row per user (their own latest month, via overall_rn_rev = 1), which is the true current figure
ds_id_summary_snapshot <- mix_databricks_read(
  query = paste0(
    "SELECT * FROM serene_data_science.05_report_layer.id_summary_snapshot ",
    "WHERE client_name = '", v_client_name, "' AND segment_type = 'overall'"
  ),
  config_db = ls_config$databricks
)

#----- Transactions

ds_transaction_distribution <- mix_databricks_read(
  query = paste0(
    "SELECT * FROM serene_data_science.05_report_layer.transaction_distribution ",
    "WHERE client_name = '", v_client_name, "' AND segment_type IN (", v_segment_types_sql_list, ") ",
    "AND month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

#----- Macro
#-- national, time-series-only indicators from feature-store-macro (R/04-model-layer/02-feature-
#-- store/02-macro-features.R -> databricks/.../02-feature-store-macro-ingest.py); gambling_premises
#-- and imd are LSOA-level (not month-keyed), so they don't apply to a month-over-month overlay and
#-- are left out here

ds_macro_cci <- mix_databricks_read(
  query = paste0(
    "SELECT month, n_cci, n_cci_diff, n_3m_avg_cci, n_cci_lag_1 ",
    "FROM serene_data_science.04_model_layer.feature_store_macro_consumer_confidence_index ",
    "WHERE month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

ds_macro_cpih <- mix_databricks_read(
  query = paste0(
    "SELECT month, n_cpih, n_cpih_diff, n_cpih_lag_1 ",
    "FROM serene_data_science.04_model_layer.feature_store_macro_cpih ",
    "WHERE month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

ds_macro_unemployment <- mix_databricks_read(
  query = paste0(
    "SELECT month, n_unemployment_rate, n_unemployment_rate_diff, n_unemployment_rate_lag_1 ",
    "FROM serene_data_science.04_model_layer.feature_store_macro_unemployment_rate ",
    "WHERE month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

ds_macro_milk <- mix_databricks_read(
  query = paste0(
    "SELECT month, n_avg_farmgate_milk_price, n_avg_farmgate_milk_price_diff, ",
    "n_3m_avg_farmgate_milk_price, n_avg_farmgate_milk_price_lag_1 ",
    "FROM serene_data_science.04_model_layer.feature_store_macro_farmgate_milk_prices ",
    "WHERE month >= '", v_min_month, "'"
  ),
  config_db = ls_config$databricks
)

#-- reshaped to one tidy long table (month, indicator, series, value) so app.R just filters/plots,
#-- rather than juggling 4 differently-shaped frames at runtime
ds_macro <- bind_rows(
  ds_macro_cci |>
    transmute(
      month, indicator = 'Consumer Confidence Index',
      Level = n_cci, `1-Month Lag` = n_cci_lag_1, `M/M Diff` = n_cci_diff, `3-Month Avg` = n_3m_avg_cci
    ) |>
    pivot_longer(cols = c('Level', '1-Month Lag', 'M/M Diff', '3-Month Avg'), names_to = 'series', values_to = 'value'),
  ds_macro_cpih |>
    transmute(
      month, indicator = 'CPIH',
      Level = n_cpih, `1-Month Lag` = n_cpih_lag_1, `M/M Diff` = n_cpih_diff
    ) |>
    pivot_longer(cols = c('Level', '1-Month Lag', 'M/M Diff'), names_to = 'series', values_to = 'value'),
  ds_macro_unemployment |>
    transmute(
      month, indicator = 'Unemployment Rate',
      Level = n_unemployment_rate, `1-Month Lag` = n_unemployment_rate_lag_1, `M/M Diff` = n_unemployment_rate_diff
    ) |>
    pivot_longer(cols = c('Level', '1-Month Lag', 'M/M Diff'), names_to = 'series', values_to = 'value'),
  ds_macro_milk |>
    transmute(
      month, indicator = 'Farmgate Milk Price',
      Level = n_avg_farmgate_milk_price, `1-Month Lag` = n_avg_farmgate_milk_price_lag_1,
      `M/M Diff` = n_avg_farmgate_milk_price_diff, `3-Month Avg` = n_3m_avg_farmgate_milk_price
    ) |>
    pivot_longer(cols = c('Level', '1-Month Lag', 'M/M Diff', '3-Month Avg'), names_to = 'series', values_to = 'value')
) |>
  filter(!is.na(value))

##### Write to data/ for app.R #####

# dir.create('data', showWarnings = F)

write_parquet(ds_id_summary, 'data/id_summary.parquet')
write_parquet(ds_id_risk_bands, 'data/id_risk_bands.parquet')
write_parquet(ds_id_distribution, 'data/id_distribution.parquet')
write_parquet(ds_id_summary_snapshot, 'data/id_summary_snapshot.parquet')
write_parquet(ds_transaction_distribution, 'data/transaction_distribution.parquet')
write_parquet(ds_macro, 'data/macro_indicators.parquet')

#-- brand/status colour palette, sourced from mixtape
saveRDS(mix_palette, 'data/mix_palette.rds')

message('✓ Data refreshed: ', length(list.files('data')), ' files written to data/')
