# get GCC and Greenness Metrics from TimeLapse imagery
# Ryan Peek, 2024

# Library -----------------------------------------------------------------

library(tidyverse)
library(purrr)
library(glue)
library(fs)
library(terra)

# Get Photo Directory --------------------------------------------

site_id <- "TahoeMFA_CRLF11" # location

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

# Create NEW mask type and number (if first of type, 01)
# if a new photo set, use DB_02_01
# if same photo set but new polygon of same veg type, use DB_01_02

mask_type <-"SH_01_02"

# read in mask
pheno_mask <- terra::rast(glue("{exif_path}/ROI/{site_id}_{mask_type}.tif"))

## TEST PLOTS --------------------------------------------------------------

# test a single photo
img <- terra::rast(glue("{photo_directory}/{photo_exif$pheno_name[47]}"))

# plot to make sure mask is in the appropriate place...if not, need to redraw
#plotRGB(img)
img <- terra::flip(img) # note if not flipped masks will extract incorrect portion

plotRGB(img)
plot(pheno_mask, col=c(NA, alpha("orange",0.8)), legend=FALSE, axes=FALSE, add=TRUE)

# Filter to 1 Photo Per Day -----------------------------------------------

photo_exif_noon <- photo_exif |>
  # this filters to only photos at noon
  filter(hms("12:00:00")==hms::as_hms(datetime))

## this filters to any photos during 12pm hour
##filter(12==hour(floor_date(datetime, unit="hour")))

# Functions to Extract Time Series of Metrics ------------------------------

# reads a single photo and generates the information/metrics of interest
ph_get_CCC <- function(path, pheno_mask, flip=TRUE){
  # read in data and crop
  img <- terra::rast(path) # path to image

  if(flip){
    # if test image above needed to be flipped
    img <- terra::flip(img)
  }
  imask <- pheno_mask # spatrast mask
  img_masked <- terra::mask(img, imask, inverse=FALSE, maskvalues=0)

  # merge and calc CCs
  img_matrix <- na.omit(as.matrix(img_masked))
  tbl <- as.data.frame(t(apply(img_matrix, 2, quantile, na.rm=TRUE, probs = c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1))))
  rownames(tbl) <- c('r','g','b')
  colnames(tbl) <- c('min','q5', 'q10','q25','q50','q75', 'q90','q95','max')
  RGB <- colMeans(img_matrix)

  t <- rowSums(img_matrix)
  ccMat <- apply(img_matrix, 2, '/', t)
  cc <- colMeans(ccMat, na.rm = TRUE)
  cc <- cc/sum(cc)

  tbl$cc <- cc
  tbl$std <- apply(ccMat, 2, sd)
  tbl$brightness <- mean(apply(img_matrix, 2, max))
  tbl$darkness <- mean(apply(img_matrix, 2, min))
  tbl$contrast <- tbl$brightness - tbl$darkness
  tbl$RGB <- RGB
  return(tbl)
}

# reads all the photos in the list and applies the above function
ph_make_CCC_ts <- function(photolist, pheno_mask){
  require(data.table)

  n <- length(photolist)
  CCCT <- matrix(NA, nrow = n, ncol = 33)
  for (i in 1:n) {
    print(paste0("Extracting CCs for ", i))
    tbl <- ph_get_CCC(photolist[i], pheno_mask)
    if (!is.null(tbl))
      CCCT[i, ] <- c(tbl$RGB, tbl$cc, tbl$std, tbl$q5,
                     tbl$q10, tbl$q25, tbl$q50, tbl$q75, tbl$q90,
                     tbl$q95, tbl$brightness[1], tbl$darkness[1],
                     tbl$contrast[1])
  }

  CCCT <- as.data.table(CCCT)
  colnames(CCCT) <- c("red", "green", "blue", "rcc", "gcc",
                      "bcc", "rcc.std", "gcc.std", "bcc.std", "rcc05", "gcc05",
                      "bcc05", "rcc10", "gcc10", "bcc10", "rcc25", "gcc25",
                      "bcc25", "rcc50", "gcc50", "bcc50", "rcc75", "gcc75",
                      "bcc75", "rcc90", "gcc90", "bcc90", "rcc95", "gcc95",
                      "bcc95", "brightness", "darkness", "contrast")
  CCCT[, `:=`(grR, gcc/rcc)]
  CCCT[, `:=`(rbR, rcc/bcc)]
  CCCT[, `:=`(gbR, gcc/bcc)]
  CCCT[, `:=`(GRVI, (gcc - rcc)/(gcc + rcc))] # green-red vegetation index
  CCCT[, `:=`(exG, (2 * green - red - blue))] # excess green
  CCCT[, `:=`(file_name, (fs::path_file(photolist)))]
  return(CCCT)
}

## Apply Functions ---------------------------------------------------------

# extract all # TAKES A WHILE!!!
system.time(df <- ph_make_CCC_ts(glue("{photo_directory}/{photo_exif_noon$pheno_name}"), pheno_mask))
beepr::beep()

# add datetime back for easier stitching
df2 <- df |> rename(pheno_name=file_name) |>
  left_join(photo_exif_noon, by= "pheno_name") |>
  #select(pheno_name, file_name:ambient_temperature, red:exG)
  select(pheno_name, file_name:ambient_temp_C, red:exG)

# Save Out ----------------------------------------------------------------

# write out zipped versions to save space
write_csv(df2, file = glue("{exif_path}/pheno_metrics_{site_id}_{mask_type}_{photo_date_dir}_midday.csv.gz"))

## Read in and Combine Series  ----------------------------------------------------------------

#mask_type <- "GR_01_02" # only if different

# read in series matching mask:
(pheno_files <- fs::dir_ls(glue("{exif_path}"),regexp = glue("{mask_type}")))

df_mids <- read_csv(file = pheno_files)

# filter to just one value at noon:
df_mid <- df_mids |>
  filter(hms("12:00:00")==hms::as_hms(datetime))

# VISUALIZATION --------------------------------------------------------------

# add bg image
library(ggimage)

# set the date to use for these plots (depends on photo set)
range(df_mid$datetime)
# here we pick 7 days prior to the last photo
photo_date_location <- max(df_mid$datetime)-days(9)

# or specify manually:
#photo_date_location <- "2024-08-21 00:00:00"

ggplot() +
  geom_smooth(data=df_mid, aes(x=datetime, y=gcc)) +
  geom_point(data=df_mid , aes(x=datetime, y=gcc),
             size=2.5, pch=21, fill="aquamarine4", alpha=0.6) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_viridis_c(option = "D", direction = -1) +
  scale_y_continuous(limits = c(0.3,0.5))+
  #scale_x_datetime(date_breaks = "2 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b") +
  labs(title=glue("Greenness Index (GCC): {site_id}"),
       subtitle= glue("A metric tracking growth using RGB values (Mask: {mask_type})"),
       x="", caption="Data from RECONYX hourly camera") +
  geom_image(
    data = tibble(datetime = ymd_hms(glue("{photo_date_location}")), gcc = .45),
    aes(x=datetime, y=gcc, image = glue("{exif_path}/ROI/{site_id}_{mask_type}_roi_masked.png")), size=0.55)

ggsave(glue("figs/gcc_{site_id}_{mask_type}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

# GRVI: look at green red veg index which tracks senescence
# expect: vegetation (index > 0), water and snow (index around 0), and soils (x < 0)
ggplot() +
  geom_smooth(data=df_mid, aes(x=datetime, y=GRVI)) +
  geom_point(data=df_mid , aes(x=datetime, y=GRVI), size=2.5, pch=21, fill="aquamarine4", alpha=0.6) +
  hrbrthemes::theme_ipsum_rc() +
  scale_y_continuous(limits = c(-0.1,0.3))+
  #scale_x_datetime(date_breaks = "2 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b") +
  labs(title=glue("Green-Red Vegetation Index (GRVI): {site_id}"),
       subtitle= glue("A metric tracking growth and senescence using RGB values (Mask: {mask_type})"),
       x="", caption="Data from RECONYX hourly camera") +
  geom_image(
    data = tibble(datetime = ymd_hms(glue("{photo_date_location}")), GRVI = 0.2),
    aes(x=datetime, y=GRVI, image = glue("{exif_path}/ROI/{site_id}_{mask_type}_roi_masked.png")), size=0.65)

ggsave(glue("figs/grvi_{site_id}_{mask_type}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

# exGREEN
ggplot() +
  geom_smooth(data=df_mid, aes(x=datetime, y=exG)) +
  geom_point(data=df_mid, aes(x=datetime, y=exG), fill="aquamarine4", size=2.5, pch=21, alpha=.6) +
  hrbrthemes::theme_ipsum_rc() +
  #scale_y_continuous(limits = c(-50,90))+
  #scale_x_datetime(date_breaks = "4 weeks", date_labels = "%Y-%b-%d") +
  scale_x_datetime(date_breaks = "1 months", date_labels = "%Y-%b") +
  labs(title=glue("Excess Greeness (exG): {site_id}"),
       subtitle= glue("A metric tracking excess greeness using RGB values (Mask: {mask_type})"),
       x="", caption="Data from RECONYX hourly camera") +
  geom_image(
    data = tibble(datetime = ymd_hms(glue("{photo_date_location}")), exG = 43),
    aes(x=datetime, y=exG, image = glue("{exif_path}/ROI/{site_id}_{mask_type}_roi_masked.png")), size=0.45)

ggsave(glue("figs/exG_{site_id}_{mask_type}_midday.png"), width = 10, height = 8, dpi = 300, bg = "white")

