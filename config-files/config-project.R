# config.R
# Central configuration for analytics engineering pipeline
#
# Usage:
#   source('config.R')
#   url <- build_azure_url(ls_config$azure, container = 'analytics', layer = 'analytics', dataset = 'monthly_tr')
#
# Note: Configuration is kept as a function (rather than just a list) to allow:
#   1. Loading different environments in the same session (e.g., compare dev vs prod)
#   2. Testing config changes without re-sourcing the entire file
#   3. Clear documentation of what parameters are configurable
#   Even though we auto-load at the bottom, the function provides flexibility.

################################################################################
##### Configuration Loader #####
################################################################################

#' Load pipeline configuration
#'
#' @param env Environment: 'prod' or 'dev' (default: prod)
#' @return List containing all configuration parameters
load_config <- function(env = 'prod') {

  ls_config <- list(

    #----- GCP Settings (auth only — still used for GCP service auth)
    gcp = list(
      auth_env_var = 'gcp-auth-js'
    ),

    #----- Azure Storage Settings
    azure = list(
      storage_account = 'sereneapplicationlayer',
      storage_key_env_var = 'azure-app-layer-storage-key',

      containers = list(

        analytics = list(
          name = 'serene-analytics-engineering',
          layers = list(
            raw = '01-raw-layer',
            clean = '02-clean-layer',
            analytics = '03-analytics-layer',
            model = '04-model-layer',
            report = '05-report-layer',
            project = '06-project-layer',
            playground = 'xx-playground-layer'
          )
        ),

        app_layer = list(
          name = 'serene-application-layer',
          layers = list(
            serene = '01-serene-data',
            client = '02-client-data'
          )
        )

      )
    ),

    #----- Google Sheets IDs
    gs_sheets = list(
      #-- Tag mappings and reference data
      serene_tags = '1GvuPAs0p4qAgCT5AKwoEDRo_KVi7iC7vC6ui9JIMou8',

      #-- Pipeline execution tracking
      code_exec_logs = '1kPMM-OOcrEUqabvxSi_DlT6Y1cHTtz4TaLqGRHT-5AI'
    ),

    #----- Parallel Processing
    parallel = list(
      # Leave N cores free for system
      cores_offset = 2,

      # Compute max cores dynamically
      max_cores = parallel::detectCores(logical = TRUE) - 2
    ),

    #----- Spark Settings
    spark = list(
      # Standard options for reading parquet files
      read_options = list(
        "recursiveFileLookup" = "true",
        "pathGlobFilter" = "*.parquet"
      )
    ),

    #----- Databricks SQL Warehouse (ODBC)
    databricks = list(
      host          = Sys.getenv('DATABRICKS_HOST'),
      http_path     = Sys.getenv('DATABRICKS_HTTP_PATH'),
      token_env_var = 'DATABRICKS_PAT',
      catalog       = 'serene_data_science',
      # Full path to the Databricks (Simba) ODBC driver .so. Used DSN-less so no
      # /etc/odbcinst.ini registration (and no extra sudo) is required.
      driver_path   = '/opt/databricks/databricksodbc/lib/64/libdatabricksodbc64.so'
    ),

    #----- Environment Metadata
    environment = env,
    loaded_at = Sys.time()
  )

  return(ls_config)
}

################################################################################
##### Helper Functions #####
################################################################################

#' Build an ADLS Gen2 abfss URL from configuration
#'
#' @param config_azure Azure config list (ls_config$azure)
#' @param container Container key: 'analytics' or 'app_layer'
#' @param layer Data layer name (e.g. 'raw', 'clean', 'analytics') or NULL for manual path
#' @param dataset Optional dataset name/subfolder
#' @return Full abfss URL: abfss://<container>@<storage_account>.dfs.core.windows.net/<path>
build_azure_url <- function(config_azure = ls_config$azure, container, layer = NULL, dataset = '') {

  container_cfg <- config_azure$containers[[container]]

  if (is.null(container_cfg)) {
    stop("Invalid container '", container, "'. Available containers: ",
         paste(names(config_azure$containers), collapse = ", "))
  }

  #-- Build path within container
  if (is.null(layer)) {
    path <- gsub('/+$', '', dataset)
  } else {
    layer_path <- container_cfg$layers[[layer]]

    if (is.null(layer_path)) {
      stop("Invalid layer '", layer, "'. Available layers: ",
           paste(names(container_cfg$layers), collapse = ", "))
    }

    path <- if (nchar(dataset) > 0) paste(layer_path, dataset, sep = '/') else layer_path
    path <- gsub('/+$', '', path)
  }

  #-- Build abfss URL
  url <- sprintf('abfss://%s@%s.dfs.core.windows.net/%s',
                 container_cfg$name,
                 config_azure$storage_account,
                 path)

  return(url)
}


#' Get Spark configuration for GCP Dataproc
#'
#' @return Configured spark_config object
get_spark_config_gcp <- function() {

  config <- sparklyr::spark_config()

  #-- Connection timeout
  config$sparklyr.connect.timeout <- 300

  #-- Unique UI port per user to avoid conflicts
  user_id <- as.numeric(Sys.getpid()) %% 1000
  config$spark.ui.port <- as.character(4040 + user_id)

  #-- ADLS Gen2 shared-key auth (required to read Azure storage from Dataproc)
  #-- spark.hadoop. prefix is required on Dataproc to propagate these into Hadoop's filesystem config
  storage_account <- ls_config$azure$storage_account
  storage_key <- Sys.getenv(ls_config$azure$storage_key_env_var)

  config[[paste0("spark.hadoop.fs.azure.account.auth.type.", storage_account, ".dfs.core.windows.net")]] <- "SharedKey"
  config[[paste0("spark.hadoop.fs.azure.account.key.", storage_account, ".dfs.core.windows.net")]] <- storage_key

  return(config)
}

#' Connect to Spark on a GCP Dataproc cluster
#'
#' @return Spark connection object
connect_spark_gcp <- function() {

  spark_cfg <- get_spark_config_gcp()

  sc <- sparklyr::spark_connect(
    master="yarn",
    spark_home="/usr/lib/spark",
    config=spark_cfg
  )

  return(sc)
}

#' Get Spark configuration for Azure HDInsight
#'
#' @return Configured spark_config object
get_spark_config_azure <- function() {

  config <- sparklyr::spark_config()

  #-- Connection timeout
  config$sparklyr.connect.timeout <- 300

  #-- Unique UI port per user to avoid conflicts
  user_id <- as.numeric(Sys.getpid()) %% 1000
  config$spark.ui.port <- as.character(4040 + user_id)

  #-- ADLS Gen2 shared-key auth (required for non-primary storage accounts)
  storage_account <- ls_config$azure$storage_account
  storage_key <- Sys.getenv(ls_config$azure$storage_key_env_var)

  config[[paste0("fs.azure.account.auth.type.", storage_account, ".dfs.core.windows.net")]] <- "SharedKey"
  config[[paste0("fs.azure.account.key.", storage_account, ".dfs.core.windows.net")]] <- storage_key

  return(config)
}

#' Connect to Spark on an Azure HDInsight cluster
#'
#' Change spark_home to /usr/hdp/current/spark2-client for Spark 2 clusters.
#'
#' @return Spark connection object
connect_spark_azure <- function() {

  spark_cfg <- get_spark_config_azure()

  sc <- sparklyr::spark_connect(
    master="yarn",
    spark_home="/usr/hdp/current/spark3-client",
    config=spark_cfg
  )

  return(sc)
}

################################################################################
##### Auto-Load Configuration #####
################################################################################

#-- Automatically load config when file is sourced
ls_config <- load_config()

#-- Set R options for pipeline
options(future.globals.maxSize = Inf)

# Print confirmation
message("✓ Configuration loaded. ", " (", ls_config$environment, ")")
