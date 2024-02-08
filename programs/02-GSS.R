# This a script clean
# GSS data
rm(list = ls())
# date: May 18th, 2022

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tictoc, parallel, pbapply, future, 
               future.apply, furrr, RhpcBLASctl, memoise, 
               here, foreign, mfx, tidyverse, hrbrthemes, 
               estimatr, ivreg, fixest, sandwich, lmtest, 
               margins, vtable, broom, modelsummary, 
               stargazer, fastDummies, recipes, dummy, 
               gplots, haven, huxtable, kableExtra, 
               gmodels, survey, gtsummary, data.table, 
               tidyfast, dtplyr, microbenchmark, ggpubr, 
               tibble, viridis, wesanderson, censReg, 
               rstatix, srvyr, formatR, sysfonts, 
               showtextdb, showtext, thematic, 
               sampleSelection, textme, paletteer, 
               wesanderson, patchwork, RStata, car,
               textme, lodown)

options("RStata.StataPath" = "/Applications/Stata/StataSE.app/Contents/MacOS/stata-se")
options("RStata.StataVersion" = 17)

# devtools::install_github("thomasp85/patchwork")
# devtools::install_github("ajdamico/lodown")
## My preferred ggplot2 plotting theme (optional)
## https://github.com/hrbrmstr/hrbrthemes
# scale_fill_ipsum()
# scale_color_ipsum()
font_add_google("Fira Sans", "firasans")
font_add_google("Fira Code", "firasans")

showtext_auto()

theme_customs <- theme(
  text = element_text(family = 'firasans', size = 16),
  plot.title.position = 'plot',
  plot.title = element_text(
    face = 'bold', 
    colour = thematic::okabe_ito(8)[6],
    margin = margin(t = 2, r = 0, b = 7, l = 0, unit = "mm")
  ),
)

colors <-  thematic::okabe_ito(5)

# theme_set(theme_minimal() + theme_customs)
theme_set(hrbrthemes::theme_ipsum() + theme_customs)
## Set master directory where all sub-directories are located
mdir <- "/Users/hhadah/Dropbox/Research/GSS"
# mdir <- "C:\\Users\\16023\\Dropbox\\Research\\My Research Data and Ideas\\Identification_Paper"
## Set working directories
workdir  <- paste0(mdir,"/Data/Datasets")
rawdatadir  <- paste0(mdir,"/Data/Raw")

## Set working directory

# COLOR PALLETES
library(paletteer) 
# paletteer_d("awtools::a_palette")
# paletteer_d("suffrager::CarolMan")

### COLOR BLIND PALLETES
paletteer_d("colorblindr::OkabeIto")
paletteer_d("colorblindr::OkabeIto_black")
paletteer_d("colorBlindness::paletteMartin")
paletteer_d("colorBlindness::Blue2DarkRed18Steps")
paletteer_d("colorBlindness::SteppedSequential5Steps")
paletteer_d("colorBlindness::PairedColor12Steps")
paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps")
colorBlindness <- paletteer_d("colorBlindness::Blue2Orange12Steps")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# scale_colour_paletteer_d("colorBlindness::ModifiedSpectralScheme11Steps", dynamic = FALSE)
# To use for fills, add
scale_fill_manual(values="colorBlindness::Blue2Orange12Steps")
scale_color_paletteer_d("nord::aurora")
scale_color_paletteer_d("colorBlindness::Blue2Orange12Steps")

# To use for line and point colors, add
scale_colour_manual(values="colorBlindness::Blue2Orange12Steps")


### read GSS data
# lodown( "gss" , output_dir = file.path(rawdatadir, "GSS" ) )
# gss_files <-
#   list.files(
#     file.path(rawdatadir, "GSS" ) ,
#     full.names = TRUE 
#   )
# gss_rds <-
#   grep( 
#     "cross sectional cumulative(.*)([0-9][0-9][0-9][0-9])\\.rds$" , 
#     gss_files , 
#     value = TRUE 
#   )
# GSS <- readRDS( gss_rds )
GSS <- read_dta("/Users/hhadah/Dropbox/Research/GSS/GSS_cum/gss7221_r2.dta")
# keep_vars <- c("year",	"id",	"age",	"race",	"region",	"sex",
#                "natrace",	"racmar",	"racdin",	
#                "racpush",	"racseg",	"racopen",
#                "racobjct",	"racschol",	"racfew",	
#                "rachaf",	"racmost",	"busing",	"racpres",	
#                "racjob",	"affrmact",	"wrkwayup",	
#                "closeblk",	"racsubs",	"racsubgv",	
#                "racmarel",	"racmarpr",	"racsups",	"racteach",
#                "racavoid",	"racchng",	"racquit",	
#                "helpblk",	"workhsps",	"intlhsps",	
#                "livehsps",	"marasian",	"marhisp",	
#                "feelblks",	"hispanic",	"ballot",	"wtssall",	
#                "vstrat",	"vpsu", "violhsps")
# GSS <- GSS[ keep_vars ]

# keep_vars <- c("year",	"id",	"age",	"race",	"region",	"sex",
#                "natrace",	"racmar",	"racdin",	
#                "racpush",	"racseg",	"racopen",
#                "racobjct",	"racschol",	"racfew",	
#                "rachaf",	"racmost",	"busing",	"racpres",	
#                "racjob",	"affrmact",	"wrkwayup",	
#                "closeblk",	"closewht", "racdif1", 
#                 "racdif2", "racdif3", "racdif4", 
#                "racsubs",	"racsubgv",	
#                "racmarel",	"racmarpr",	"racsups",	"racteach",
#                "racavoid",	"racchng",	"racquit",	
#                "helpblk",	"workhsps",	"intlhsps",	
#                "livehsps",	"marasian",	"marhisp",	
#                "feelblks",	"hispanic",	"ballot",	"wtssall",	
#                "vstrat",	"vpsu", "violhsps")
# GSS <- GSS[ keep_vars ]

names(GSS) <- toupper(names(GSS))
GSS <- GSS %>% 
  mutate(RACFEW = as.factor(RACFEW)) %>% 
  rename("year" = "YEAR",
         "id" = "ID")
### read Geocode data
Geocode <- read_dta(paste0(rawdatadir,"/cross7214_geo.dta"))

### Merge GSS and geocode
Geocode_GSS <- left_join(
  GSS,
  Geocode,
  na_matches = "never",
  by = c("year", "id"))

# Geocode_GSS <- Geocode_GSS
# %>% 
#   filter(!is.na(fipsstat))

write.csv(Geocode_GSS, paste0(workdir,"/Geocoded_GSS.csv"))
