#05 Time Series Analysis

## WORK IN PROGRESS

# Library -----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(glue)
library(fs)
library(terra)

# Get Photo Directory --------------------------------------------

site_id <- "KWA_BDR16" # location

# Full path to folder where photos are located
# this function helps select the folder and ensures there are images in the folder to use
select_dir <- function(){
  print("Select any image file WITHIN the folder you want to use:")
  dirname(file.choose(new = FALSE))
}

# select an image from inside the folder of interest
photo_directory <- select_dir()
photo_directory

# Read Photo Metadata -----------------------------------------------------

# create path to drive location:
photo_date_dir <- basename(photo_directory)
exif_path <- fs::path_dir(photo_directory)

# read in the exif metadata (run via 02_extract_metadata)
photo_exif <- read_csv(glue("{exif_path}/pheno_exif_{site_id}_{photo_date_dir}.csv.gz"))

# Get Mask ----------------------------------------------------------------

mask_type <-"WA_01_01"

# read in series matching mask:
(pheno_files <- fs::dir_ls(glue("{exif_path}"),regexp = glue("{mask_type}")))

# filter
df_mids <- read_csv(file = pheno_files)

# filter to just one value at noon:
df_mid <- df_mids |>
  filter(hms("12:00:00")==hms::as_hms(datetime))


# TIMESERIES ANALYSIS ------------------------------------------------------------------

# read and extract site only: str_extract(head(df_mid$pheno_name), "[A-Z]+\\d{1}")

library(timetk)

## GRVI: Different time periods ------------------------------------------

df_mid |>
  plot_time_series_boxplot(datetime, GRVI, .period = "3 day",  .interactive = FALSE, .title = glue("GRVI at {site_id}: 3 day"))

df_mid  |>
  plot_time_series_boxplot(datetime, GRVI, .period = "1 week",  .interactive = FALSE, .title = glue("GRVI at {site_id}: 1 week"))

df_mid |>
  plot_time_series_boxplot(datetime, GRVI, .period = "14 day",  .interactive = FALSE, .title = glue("GRVI at {site_id}: 14 days"))

## GCC: Different time periods ------------------------------------------

df_mid |>
  plot_time_series_boxplot(datetime, gcc, .period = "3 day",  .interactive = FALSE, .title = glue("GCC at {site_id}: 3 days"))

df_mid |>
  plot_time_series_boxplot(datetime, gcc, .period = "1 week",  .interactive = FALSE, .title = glue("GCC at {site_id}: 1 week"))

df_mid |>
  plot_time_series_boxplot(datetime, gcc, .period = "1 month",  .interactive = FALSE, .title = glue("GCC at {site_id}: 1 month"))


# plot ACF diagnostics
df_mid |> plot_acf_diagnostics(datetime, gcc, .lags= "2 weeks", .interactive = FALSE)
df_mid |> plot_acf_diagnostics(datetime, gcc, .lags= "1 month", .interactive = FALSE)
df_mid |> plot_acf_diagnostics(datetime, GRVI, .lags= "2 weeks", .interactive = FALSE)
df_mid |> plot_acf_diagnostics(datetime, GRVI, .lags= "1 month", .interactive = FALSE)
df_mid |> plot_acf_diagnostics(datetime, exG, .lags= "2 weeks", .interactive = FALSE)
df_mid |> plot_acf_diagnostics(datetime, exG, .lags= "1 month", .interactive = FALSE)



df_mid |>  plot_seasonal_diagnostics(datetime, gcc, .feature_set = c("week", "month.lbl", "quarter"))
df_mid |>  plot_seasonal_diagnostics(datetime, GRVI, .feature_set = c("week", "month.lbl", "quarter"))
df_mid |>  plot_seasonal_diagnostics(datetime, exG, .feature_set = c("week", "month.lbl", "quarter"))

df_mid |> plot_anomaly_diagnostics(datetime, gcc)
df_mid |> plot_anomaly_diagnostics(datetime, GRVI)
df_mid |> plot_anomaly_diagnostics(datetime, exG)
