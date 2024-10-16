#05 Time Series Analysis

## WORK IN PROGRESS

## pull all data and plot together
## look for trend/timepoints in plots to get a sense of how timing may be different for green up/senescence
## how to compare "relative" values vs absolute GCC, GRVI, etc

# Library -----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(glue)
library(fs)
library(terra)
library(readxl)
library(purrr)

# Get Sites ---------------------------------------------------------------

# can look in folder, or read in metadata and create list:

username <- Sys.getenv("USERNAME")
drive <- r'(C:\Users\)'
onedrive <- r'(OneDrive - California Department of Fish and Wildlife\)'
year <- 2024
data_path <- glue("{drive}/{username}/{onedrive}/Terrestrial/Data/{year}/")
meta <- read_xlsx(glue("{data_path}/Timelapse_Camera/timelapse_metadata_pheno.xlsx"))

# get all site ids
site_ids <- meta$site_id # or create your own list!

# site_id <- "COLE1" #single id

# use fs to check if these directories have the file we want?
file_list <- map(site_ids, ~fs::dir_ls(glue("E:/cemap_pheno/{.x}"), regexp = "pheno_metrics")) |>
  purrr::compact() |> # drops empty values
  list_c() # returns a list

# Read Metric Data -----------------------------------------------------

# read in all the data!!
photo_metrics <- read_csv(glue("{file_list}"), id = "filename") |>
  mutate(site_id = basename(fs::path_dir(filename)), .after="filename") |>
  # add mask info:
  mutate(mask = str_extract(basename(filename),
                            pattern=glue("(?!_{site_id}_)[A-Z]{{2}}_[0-9]{{2}}_[0-9]{{2}}")),
         .after="site_id") |>
  mutate(mask_type = substr(mask, 1,2), .before="mask")

table(photo_metrics$mask_type)

# Plot All  ---------------------------------------------------------------

# filter to same mask type and then plot:
mask_sel <- "DB"
df_filt <- photo_metrics |> filter(mask_type==mask_sel)

## GCC ---------------------------------------------------------------------

ggplot() +
  geom_smooth(data=df_filt, aes(x=datetime, y=gcc, fill=site_id), color=alpha("gray30",0.5)) +
  geom_point(data=df_filt , aes(x=datetime, y=gcc, fill=site_id),
             size=2.5, pch=21, alpha=0.6) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_viridis_d("Site ID", option = "D", direction = -1) +
  #scale_y_continuous(limits = c(0.3,0.5))+
  #scale_x_datetime(date_breaks = "2 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b-%d") +
  labs(title=glue("Greenness Index (GCC):"),
       subtitle= glue("A metric tracking growth using RGB values (Mask: {mask_sel})"),
       x="", caption="Data from RECONYX hourly camera") #+
  #facet_wrap(.~site_id, scales = "free_y")

ggsave(glue("figs/gcc_all_sites_mask_{mask_sel}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

## GRVI -------------------------------------------------------------------
# look at green red veg index which tracks senescence
# expect: vegetation (index > 0), water and snow (index around 0), and soils (x < 0)

ggplot() +
  geom_smooth(data=df_filt, aes(x=datetime, y=GRVI, fill=site_id),  color=alpha("gray30",0.5)) +
  geom_point(data=df_filt , aes(x=datetime, y=GRVI, fill=site_id),
             size=2.5, pch=21, alpha=0.6) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_viridis_d("Site ID", option = "D", direction = -1) +
  #scale_y_continuous(limits = c(-0.1,0.3))+
  #scale_x_datetime(date_breaks = "2 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b") +
  labs(title=glue("Green-Red Vegetation Index (GRVI):"),
       subtitle= glue("A metric tracking growth and senescence using RGB values (Mask: {mask_sel})"),
       x="", caption="Data from RECONYX hourly camera")

ggsave(glue("figs/grvi_all_sites_mask_{mask_sel}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

## exGREEN ---------------------------------------------------------------

ggplot() +
  geom_smooth(data=df_filt, aes(x=datetime, y=exG,  fill=site_id),  color=alpha("gray30",0.5)) +
  geom_point(data=df_filt, aes(x=datetime, y=exG, fill=site_id),
             size=2.5, pch=21, alpha=.6) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_viridis_d("Site ID", option = "D", direction = -1) +
  #scale_y_continuous(limits = c(-50,90))+
  #scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b") +
  labs(title=glue("Excess Greeness (exG):"),
       subtitle= glue("A metric tracking excess greeness using RGB values (Mask: {mask_sel})"),
       x="", caption="Data from RECONYX hourly camera")

ggsave(glue("figs/exG_all_sites_mask_{mask_sel}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

# TIMESERIES ANALYSIS ------------------------------------------------------------------

library(timetk)

## GRVI: Different time periods ------------------------------------------

df_filt |>
  plot_time_series_boxplot(datetime, GRVI, .period = "3 day",  .interactive = FALSE, .title = glue("GRVI: 3 day"))

df_filt  |>
  plot_time_series_boxplot(datetime, GRVI, .period = "1 week",  .interactive = FALSE, .title = glue("GRVI: 1 week"))

df_filt |>
  plot_time_series_boxplot(datetime, GRVI, .period = "14 day",  .interactive = FALSE, .title = glue("GRVI: 14 days"))

## GCC: Different time periods ------------------------------------------

df_filt |>
  plot_time_series_boxplot(datetime, gcc, .period = "3 day",  .interactive = FALSE, .title = glue("GCC: 3 days"))

df_filt |>
  plot_time_series_boxplot(datetime, gcc, .period = "1 week",  .interactive = FALSE, .title = glue("GCC: 1 week"))

df_filt |>
  plot_time_series_boxplot(datetime, gcc, .period = "1 month",  .interactive = FALSE, .title = glue("GCC: 1 month"))


# plot ACF diagnostics
df_filt |> plot_acf_diagnostics(datetime, gcc, .lags= "2 weeks", .interactive = FALSE)
df_filt |> plot_acf_diagnostics(datetime, gcc, .lags= "1 month", .interactive = FALSE)
df_filt |> plot_acf_diagnostics(datetime, GRVI, .lags= "2 weeks", .interactive = FALSE)
df_filt |> plot_acf_diagnostics(datetime, GRVI, .lags= "1 month", .interactive = FALSE)
df_filt |> plot_acf_diagnostics(datetime, exG, .lags= "2 weeks", .interactive = FALSE)
df_filt |> plot_acf_diagnostics(datetime, exG, .lags= "1 month", .interactive = FALSE)


# Seasonal
df_filt |>  plot_seasonal_diagnostics(datetime, gcc, .facet_vars = "site_id",
                                      .feature_set = c("week", "month.lbl", "quarter"))
df_filt |>  plot_seasonal_diagnostics(datetime, GRVI,
                                      .facet_vars = "site_id",
                                      .feature_set = c("week", "month.lbl", "quarter"))
df_filt |>  plot_seasonal_diagnostics(datetime, exG,
                                      .facet_vars = "site_id",
                                      .feature_set = c("week", "month.lbl", "quarter"))

df_filt |> plot_anomaly_diagnostics(datetime, gcc)
df_filt |> plot_anomaly_diagnostics(datetime, GRVI)
df_filt |> plot_anomaly_diagnostics(datetime, exG)
