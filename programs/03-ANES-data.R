# This a script to 
# clean ANES data

# Date: Feb 16th, 2023

### Open ANES data
### and rename some variables 
ANES <- read_csv(ANES_dir) |> 
    # clean_names() |> 
    rename(year             = VCF0004,
           statefip             = VCF0901a,
           stae_ab          = VCF0901b,
           gender           = VCF0104,
           age              = VCF0103,
           race_det         = VCF0105a,
           race             = VCF0105b,
           race_sum         = VCF0106,
           educ             = VCF0110,
           party_identity   = VCF0303,
           race_res_1       = VCF9040,
           race_res_2       = VCF9039,
           race_res_3       = VCF9042,
           race_res_4       = VCF9041,
           feelings_hisp    = VCF0217,
           feelings_blk     = VCF0206,
           feelings_wmn     = VCF0225,
           feelings_asn     = VCF0227,
           ) |> 
    select(
        year, statefip, stae_ab, 
        gender, age, race_det, race_det, race_sum, educ, party_identity,
        race_res_1, race_res_2, race_res_3, race_res_4, 
        feelings_hisp, feelings_blk, feelings_wmn, feelings_asn
    ) |> 
    glimpse()

# Create racial animosity var
ANES <- ANES |>
  mutate(race_res_1_rec = case_when(race_res_1 == 1   ~ 4/4,
                                    race_res_1 == 2   ~ 3/4,
                                    race_res_1 == 3   ~ 2/4,
                                    race_res_1 == 4   ~ 1/4,
                                    race_res_1 == 5   ~ 0,
                                    TRUE ~ NA_real_),
         race_res_2_rec = case_when(race_res_2 == 1   ~ 0,
                                    race_res_2 == 2   ~ 1/4,
                                    race_res_2 == 3   ~ 2/4,
                                    race_res_2 == 4   ~ 3/4,
                                    race_res_2 == 5   ~ 4/4,
                                    TRUE ~ NA_real_),
         race_res_3_rec = case_when(race_res_3 == 1   ~ 0,
                                    race_res_3 == 2   ~ 1/4,
                                    race_res_3 == 3   ~ 2/4,
                                    race_res_3 == 4   ~ 3/4,
                                    race_res_3 == 5   ~ 4/4,
                                    TRUE ~ NA_real_),
         race_res_4_rec = case_when(race_res_4 == 1   ~ 4/4,
                                    race_res_4 == 2   ~ 3/4,
                                    race_res_4 == 3   ~ 2/4,
                                    race_res_4 == 4   ~ 1/4,
                                    race_res_4 == 5   ~ 0,
                                    TRUE ~ NA_real_)                               
        )
ANES <- ANES |>
  mutate(race_anim = rowMeans(ANES[c("race_res_1_rec", "race_res_2_rec", "race_res_3_rec", "race_res_4_rec")], na.rm = T))

# Create a dataframe with the mean racial animosity by state and year
ANES_grouped_bystate_year <- ANES %>% 
  group_by(statefip, year) %>%
  summarise(Mean_AggregateIndex = mean(race_anim, na.rm = TRUE)) %>% 
  select(statefip, 
         Mean_AggregateIndex,
         year) |> 
  mutate(state = fips(statefip, to = "Name"))

# create a dataframe with the state names in lowercase
# and remove the years with NA raceanon values
# and create a join_year variable to join with CPS data
ANES_grouped_bystate_year <- ANES_grouped_bystate_year |> 
  mutate(state = tolower(state),
         Join_year = year) |> 
  # rename(year_anes = year) |> 
  filter(!is.na(Mean_AggregateIndex))

ANES_grouped_bystate_year  <- ANES_grouped_bystate_year |> 
  mutate(Trump_2015 = case_when(year <= 2015 ~ 0,
                                year >  2015 ~ 1,
                                ))
feols(Mean_AggregateIndex ~ Trump_2015 | statefip, data = ANES_grouped_bystate_year, vcov = ~statefip)
# this "states" dataframe has the lat & long info needed for mapping.
states <- st_as_sf(map('state', plot = TRUE, fill = TRUE))
states <- states %>% 
  rename(state = ID)

# states <- map_data("state")

# join ANES + lowercase names to df that has lat & long
ANES_bystate_year_map <- inner_join(ANES_grouped_bystate_year, 
                                        states, 
                                        by = "state") 
ANES_bystate_year_map <- st_as_sf(ANES_bystate_year_map)


library(tigris)

sts <- states() |> 
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP'))

DIVISION <- sts %>%
  group_by(DIVISION) %>% 
  summarize()

# use for loop to plot all maps

for (year_map in c(1986, 1988, 
                   1990, 1992, 1994, 1998,
                   2000, 2004, 2016, 2020)) {
  map <- ggplot() + geom_sf(data = ANES_bystate_year_map |> filter(year_anes == year_map), 
                            aes(fill = Mean_AggregateIndex), 
                            color = "white")+
    geom_sf(data = ANES_bystate_year_map, 
            color = 'white', 
            fill = NA,
            size = 0.01) +
    geom_sf(data = DIVISION, 
            color = 'red', 
            fill = NA,
            size = 0.9) +
    scale_fill_viridis_c(option = "D", direction = -1, name = "Bias"#,
                         #breaks = seq(-0.4,0.4,0.2)
                         ) +
    theme_customs_map() +
    theme(legend.position = "bottom")
  map
  ggsave(path = figures_wd, filename = paste0(year_map,"ANES-map.png"))
  ggsave(path = "~/Documents/GiT/Attitudes-and-Identity/my_paper/figure", 
         filename = paste0(year_map,"ANES-map.png"), width = 8, height = 5, 
         units = c("in"))
  
}
### Open CPS data
### of 17 year olds
### living with their
### parents

CPS <- fread(CPS_path)
CPS <- as.data.frame(CPS)

# create a join year varaible for CPS
# to join with ANES
# and create a respondent type variable

CPS <- CPS %>% 
  mutate(
         Join_year = case_when(
          year == 1994 ~ 1994,
          year >= 1995 & year <=  1998 ~ 1998,
          year >= 1999 & year <=  2000 ~ 2000,
          year >= 2001 & year <=  2004 ~ 2004,
          year >= 2005 & year <=  2008 ~ 2008,
          year >= 2009 & year <=  2012 ~ 2012,
          year >= 2013 & year <=  2016 ~ 2016,
          year >= 2017 & year <=  2020 ~ 2020
         ),
         Proxy = case_when(hhrespln ==lineno ~ "Self",
                           hhrespln ==lineno_mom ~ "Mother",
                           hhrespln ==lineno_mom2 ~ "Mother",
                           hhrespln ==lineno_pop  ~ "Father",
                           hhrespln ==lineno_pop2 ~ "Father",
                           TRUE ~ "Other"))
### Merge ANES and CPS
# create female and parent type variables
CPS_index <- left_join(CPS,
                       ANES_grouped_bystate_year,
                       na_matches = "never",
                       by = c("statefip", "year")) %>% 
  filter(!is.na(statefip) & !is.na(Mean_AggregateIndex)) %>% 
  mutate(Type = case_when(Type == "First Generation" ~ "First Generation",
                             Type == "Second Generation" ~ "Second Generation",
                             Type == "Third Generation" ~ "Third Generation"),
         Female = case_when(sex == 2 ~ 1,
                            sex == 1 ~ 0))
# create age, age^2, age^3, age^4 variables
# create Hispanic parent type variables
# create Objective Hispanic parent type variables  
# create Hispanic parent type variables
# create Objective Hispanic parent type variables
# create grandparents variables
# create weight variables
CPS_index <- CPS_index |> 
  filter(Type != "Fourth Generation+ Hispanic" | !is.na(Type)) |> 
  mutate(ftotval_mom = ifelse(ftotval_mom <= 1, 1, ftotval_mom),
         lnftotval_mom = log(ftotval_mom),
         Age = age,
         Age_sq = age^2,
         Age_cube = age^3,
         Age_quad = age^4,
         HH = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 1, 1, 0),
         HW = ifelse(Hispanic_Dad == 1 & Hispanic_Mom == 0, 1, 0),
         WH = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 1, 1, 0),
         WW = ifelse(Hispanic_Dad == 0 & Hispanic_Mom == 0, 1, 0),
         HH_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         HW_0bj = ifelse((SpanishSpeakingPOB_Father == 1 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         WH_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 1), 1, 0),
         WW_0bj = ifelse((SpanishSpeakingPOB_Father == 0 & SpanishSpeakingPOB_Mother == 0), 1, 0),
         ParentType = case_when(HH == 1 ~ "Hispanic-Hispanic",
                                HW == 1 ~ "Hispanic-White",
                                WH == 1 ~ "White-Hispanic",
                                WW == 1 ~ "White-White"),
         ParentType = as.factor(ParentType),
         ParentType2 = case_when(HH_0bj == 1 ~ "Hispanic-Hispanic",
                                 HW_0bj == 1 ~ "Hispanic-White",
                                 WH_0bj == 1 ~ "White-Hispanic",
                                 WW_0bj == 1 ~ "White-White"),
         ParentType2 = as.factor(ParentType2),
         
         HH_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 1 | SpanishSpeakingPOB_PatGrandFather == 1) 
                           & (SpanishSpeakingPOB_MatGrandMother == 1 | SpanishSpeakingPOB_MatGrandFather == 1), 1, 0),
         HW_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 1 | SpanishSpeakingPOB_PatGrandFather == 1) 
                           & (SpanishSpeakingPOB_MatGrandMother == 0 & SpanishSpeakingPOB_MatGrandFather == 0), 1, 0),
         WH_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 0 & SpanishSpeakingPOB_PatGrandFather == 0) 
                           & (SpanishSpeakingPOB_MatGrandMother == 1 | SpanishSpeakingPOB_MatGrandFather == 1), 1, 0),
         WW_0bj_3 = ifelse((SpanishSpeakingPOB_PatGrandMother == 0 & SpanishSpeakingPOB_PatGrandFather == 0) 
                           & (SpanishSpeakingPOB_MatGrandMother == 0 & SpanishSpeakingPOB_MatGrandFather == 0), 1, 0),
         ParentType3 = case_when(HH == 1 ~ "Hispanic-Hispanic",
                                 HW == 1 ~ "Hispanic-White",
                                 WH == 1 ~ "White-Hispanic",
                                 WW == 1 ~ "White-White"),
         ParentType3 = as.factor(ParentType3),
         
         Grandparent_Type = case_when((SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WWWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ "WWWH",
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WWHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WWHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WHWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WHWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'WHHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 0 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'WHHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HWWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HWWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HWHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 0) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HWHH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HHWW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 0 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HHWH',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 0) ~ 'HHHW',
                                      (SpanishSpeakingPOB_PatGrandFather == 1 & SpanishSpeakingPOB_PatGrandMother == 1) 
                                      & (SpanishSpeakingPOB_MatGrandFather == 1 & SpanishSpeakingPOB_MatGrandMother == 1) ~ 'HHHH'
         ),
         Grandparent_Type = as.factor(Grandparent_Type),
         weight = case_when(!is.na(hwtfinl) ~ hwtfinl,
                            !is.na(asecfwt) ~ asecfwt,
                            !is.na(asecwt04) ~ asecwt04))

write_csv(CPS_index, file.path(datasets,"CPS_index_anes.csv"))
# 
# # remove datasets
# rm(list = c("CPS", "CPS_index", "ANES_bystate"))


