############################################ START ###########################################################

############################################ preparedata.R ################################################

#### Queries: UNHCR Statistics and Demographics Section, UNICEF
#### Project: Demographic models end-2022
#### Description: Data preparation of ASR 2022 REF + OIP demographic data for imputation model

##### I. Read data, packages etc #####

### packages
library(tidyverse)
library(stringi)
library(readxl)
library(writexl)


### read data

demsubn <- read_csv("data/Export_Demographics_Locations.csv")


## country and region codes and names
m49 <- read_excel("data/UNSD — Methodology.xlsx") # m49 codes and regions from https://unstats.un.org/unsd/methodology/m49/overview/
countries <- read_excel("data/World_Bureaus.xlsx") # iso codes with UNHCR regions

## neighbour matrix
load("data/neighbor.RData")

## distance matrix
load("data/distance.RData")
rm(distance_matrix)

## GNI difference matrix
load("data/GNI_diff.RData")


##### II. Check demographic data and create variables for missingness structure #####


### first check of demographic data

summary(demsubn) # x_unknown variables are empty, whereas x_other are populated
glimpse(demsubn) # confirmed by DAS team: x_other are age unknowns


### clean variable names in demographic dataset
demsubn <- demsubn %>%
  rename(
    year = Year,
    popType = PT,
    female_0_4 = 'Female_0_4',
    female_5_11 = 'Female_5_11',
    female_12_17 = 'Female_12_17',
    female_18_59 = 'Female_18_59',
    female_60 = 'Female_60',
    female_AgeUnknown = 'Female_Unknown',
    female = 'Female_total',
    male_0_4 = 'Male_0_4',
    male_5_11 = 'Male_5_11',
    male_12_17 = 'Male_12_17',
    male_18_59 = 'Male_18_59',
    male_60 = 'Male_60',
    male_AgeUnknown = 'Male_Unknown',
    male = 'Male_total',
    totalEndYear = "Total"
  )

dem <- demsubn %>%
  filter(year == 2022) %>%
  filter(popType %in% c("REF", "OIP"))

dem <- dem %>%
  group_by(year, asylum, origin, popType, AggregationType) %>%
  summarise(across(female_0_4:totalEndYear, sum)) %>%
  ungroup()

summary(dem)


### check missing age and sex counts and totals in demographic dataset

## sex
dem <- dem %>%
  mutate(
    knownSexSum = rowSums(select(.,female, male), na.rm = T),
    sexAge_unknown = totalEndYear-knownSexSum # count of sex and age unknown in this asylum/origin/popType combination
  ) %>%
  mutate(
    sexSum = rowSums(select(., female, male, sexAge_unknown), na.rm = T), # check of all possible counts for sex (female, male, unknown)
    sexDiff = totalEndYear - sexSum # should be 0 (all accounted for in female, male or unknown)
  )

summary(dem$sexDiff) # OK (all 0)

dem <- dem %>%
  mutate(
    female_AgeKnown = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60), na.rm = T)
  ) %>%
  mutate(
    femaleAgeSum = rowSums(select(., female_AgeKnown, female_AgeUnknown), na.rm = T),
    femaleAgeDiff = female - femaleAgeSum # should be 0 (all accounted for in age or unknown age)
  ) %>%
  mutate(
    male_AgeKnown = rowSums(select(., male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T)
  ) %>%
  mutate(
    maleAgeSum = rowSums(select(., male_AgeKnown, male_AgeUnknown), na.rm = T),
    maleAgeDiff = male - maleAgeSum # should be 0 (all accounted for in age or unknown age)
  )

summary(dem$femaleAgeDiff) # OK
summary(dem$maleAgeDiff) # OK


dem <- dem %>%
  mutate(
    age_unknown = rowSums(select(., female_AgeUnknown, male_AgeUnknown), na.rm = T), # count of age unknown, sex known in this asylum/origin/popType combination
    sexAge_known = rowSums(select(., female_AgeKnown, male_AgeKnown), na.rm = T) # count of age and sex known in this asylum/origin/popType combination
  ) %>%
  mutate(
    knownUnknownSum = rowSums(select(., sexAge_known, age_unknown, sexAge_unknown))
  ) %>%
  mutate(
    knownUnknownDiff = totalEndYear - knownUnknownSum # should be all 0
  )


summary(dem$knownUnknownDiff) # OK (all 0)

# delete check variables

dem <- dem %>%
  select(-c(knownSexSum, sexSum, sexDiff, femaleAgeSum, femaleAgeDiff, maleAgeSum, maleAgeDiff,
            knownUnknownSum, knownUnknownDiff))



##### III. Merge m49 and UNHCR country information files into dataset m49hcr #####


### merge m49 dataset with UNHCR region codes and clean variable names

# Stateless and Tibet: keep structure including those two codes in imputed dataset
# covariates and regions: use those for China for Tibet/Taiwan, Serbia for Kosovo. For XXA and NAA, set region to own "unknown" level, use means of covariates (distance and GDP)


## add missing origins / asylum countries to m49 dataset
m49 <- m49 %>%
  rename(
    region="Region Name",
    subregion="Sub-region Name",
    country = "Country or Area",
    m49 = "M49 Code",
    iso3 = "ISO-alpha3 Code"
  )  %>%
  select(region, subregion, country, m49, iso3) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TIB", country = "Tibet", m49 = 156)) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TWN", country = "Taiwan", m49 = 158)) %>%
  add_row(region = "Unknown", subregion = "Unknown", country = "Stateless", iso3 = "XXA", m49 = 997) %>%
  add_row(region = "Unknown", subregion = "Unknown", country = "Unknown", iso3 = "NAA", m49 = 998) %>%
  add_row(filter(., iso3 == "SRB") %>% mutate(iso3 = "XKX", country = "Kosovo", m49 = 412))


## add missing origins / asylum countries to countries dataset
countries <- countries %>%
  select(iso3, proGres_code, main_office_short, hcr_region, hcr_subregion) %>%
  add_row(filter(., iso3 == "CHN") %>% mutate(iso3 = "TIB",  proGres_code = "TIB")) %>%
  add_row(main_office_short = "Unknown", hcr_region = "Unknown", hcr_subregion = "Unknown", iso3 = "XXA", proGres_code = "STA") %>%
  add_row(main_office_short = "Unknown", hcr_region = "Unknown", hcr_subregion = "Unknown", iso3 = "NAA", proGres_code = "UKN")


## create merge of m49 and countries files
dim(m49)
dim(countries)

m49hcr <- m49 %>%
  left_join(countries, by = "iso3")

dim(m49hcr) # OK
sum(duplicated(m49hcr$iso3)) # OK, no duplicates








##### IV. Merge demographic dataset with m49hcr #####

dim(dem)
dim(m49hcr)

## create m49hcr versions for origin and asylum country

m49hcr_asylum <- m49hcr %>%
  rename_with( ~ paste0("asylum_", .))

m49hcr_origin <- m49hcr %>%
  rename_with( ~ paste0("origin_", .))


## merge with demographics dataset

dem <- dem %>%
  left_join(m49hcr_asylum, by = c("asylum" = "asylum_proGres_code")) %>%
  left_join(m49hcr_origin,by = c("origin" = "origin_proGres_code"))
dim(dem) # OK

# check whether NAs in asylum/origin variables

table(dem$asylum_country, useNA = "ifany")
table(dem$asylum_region, useNA = "ifany")
table(dem$asylum_iso3, useNA = "ifany") # no NAs
table(dem$origin_country, useNA = "ifany") # 93 NAs
table(dem$origin_region, useNA = "ifany") # 93 NAs
table(dem$origin_iso3, useNA = "ifany") # 93 NAs

# View(dem %>% filter(is.na(origin_iso3))) # STA, UKN, TIB

## check whether there are origin or asylum codes in demo data that are not in m49
unhcr_iso3 <- unique(c(dem$origin_iso3, dem$asylum_iso3))

unhcr_iso3_missingM49 <- unhcr_iso3[!(unhcr_iso3 %in% m49$iso3)] # OK, none
unhcr_iso3_missingCountries <- unhcr_iso3[!(unhcr_iso3 %in% countries$iso3)] # Ok, none

countries_iso3_missingM49 <- countries$iso3[!(countries$iso3 %in% m49$iso3)] # Ok, none




##### V. Merge distance and GNI covariates to dataset #####

### checks: are all origin/asylum combinations from dem available in distance, neighbour and GNI datasets without missings?

pairs_iso3 <- unique(dem %>% unite("pairs_iso3", origin_iso3, asylum_iso3) %>% select(pairs_iso3)) # all origin-asylum pairs in dem


## check neighbours
# are all origins in the neighbour matrix?
neighbour_ori_iso3 <- unique(all.neighbors$country_iso3)
check_neighbour_ori <- dem %>%
  filter(!(origin_iso3 %in% neighbour_ori_iso3)) # OK (islands, stateless, unknown)

neighbour_asy_iso3 <- unique(all.neighbors$neighbor_iso3)
check_neighbour_asy <- dem %>%
  filter(!(asylum_iso3 %in% neighbour_asy_iso3)) # OK (islands)


## check distance
distance_pairs <- unique(unlist(distance_df_long %>% unite("pairs_iso3", orig_iso3, dest_iso3) %>% select(pairs_iso3)))

check_distance <- pairs_iso3 %>%
  filter(!(pairs_iso3 %in% distance_pairs)) # OK (only stateless and unknown origin missing in distance matrix)

## check gni
gni_pairs <- unique(unlist(gni_diff_long %>% unite("pairs_iso3", orig_iso3, dest_iso3) %>% select(pairs_iso3)))

check_gni <- pairs_iso3 %>%
  filter(!(pairs_iso3 %in% gni_pairs)) # OK (only stateless and unknown origin missing in gni matrix)


### merge covariates to dem

dim(dem)
dem <- dem %>%
  unite("pairs_iso3", origin_iso3, asylum_iso3, remove = F) %>%
  left_join(
    all.neighbors %>% # neighbours
      unite("pairs_iso3", country_iso3, neighbor_iso3) %>%
      mutate(neighbor = "Yes"),
    by = "pairs_iso3"
  ) %>%
  mutate(neighbor = replace_na(neighbor, "No")) %>%
  left_join( # distance
    distance_df_long %>%
      select(orig_iso3, dest_iso3, distance) %>%
      unite("pairs_iso3", orig_iso3, dest_iso3),
    by = "pairs_iso3"
  ) %>%
  left_join( # GNI difference and origin
    gni_diff_long %>%
      select(orig_iso3, dest_iso3, orig_gni, gni_diff) %>%
      rename(gni_origin = orig_gni) %>%
      unite("pairs_iso3", orig_iso3, dest_iso3),
    by = "pairs_iso3"
  )  %>%
  left_join( # GNI by asylum
    gni_diff_long %>%
      select(dest_iso3, dest_gni) %>%
      distinct() %>%
      rename(gni_asylum = dest_gni),
    by = c("asylum_iso3"="dest_iso3")
  )
dim(dem)

### for missing values of distance, GNI by origin and GNI difference (stateless and unknown origins), impute population-weighted mean of refugees/VDA with known origin by country of asylum

dem <- dem %>%
  group_by(asylum_iso3) %>%
  mutate(distanceAsyMeanW = weighted.mean(distance, w=totalEndYear, na.rm=T),
         gni_originAsyMeanW = weighted.mean(gni_origin, w=totalEndYear, na.rm=T),
         gni_diffAsyMeanW = weighted.mean(gni_diff, w=totalEndYear, na.rm=T)
        ) %>%
  ungroup() %>%
  mutate(distance = case_when(
           is.na(distance) | distance == 0 ~ distanceAsyMeanW,
           !(is.na(distance) | distance == 0 ) ~ distance
          ),
         gni_origin = case_when(
           is.na(gni_origin) ~ gni_originAsyMeanW,
           !(is.na(gni_origin)) ~ gni_origin
         ),
         gni_diff = case_when(
           is.na(gni_diff) ~ gni_diffAsyMeanW,
           !(is.na(gni_diff)) ~ gni_diff
         )
  ) %>%
  select(-c(distanceAsyMeanW, gni_originAsyMeanW, gni_diffAsyMeanW))

dim(dem %>% filter(is.na(distance)|is.na(gni_asylum)|is.na(gni_origin)|is.na(gni_diff))) # OK (empty)


##### VI. Create long dataset with one row per asylum/origin/poptype combination and missingness type #####

dem_longMissing <- dem %>%
  pivot_longer(cols = c(sexAge_known, age_unknown, sexAge_unknown), # pivot to get separate missing/non-missing parts of dataframe for fit and predictions
              names_to = "missing",
              values_to = "total") %>%
  mutate(missing = case_when( # sensible values for missing variable
      missing == "sexAge_unknown" ~ "sexAge",
      missing == "age_unknown" ~ "age",
      missing == "sexAge_known" ~ "none"
    )
  ) %>%
  filter(total > 0) %>% # keep only non-zero rows
  mutate(across(c(male_0_4:male_60, female_0_4:female_60), # set age variables to NA if age unknown
            ~ case_when(
              missing == "sexAge" ~ NA_real_,
              missing == "age" ~ NA_real_,
              missing == "none" ~ .
            )
        )
  ) %>%
  mutate(female = case_when( # set sex variables to NA if sex unknown, to number of unknown age in sex category if age only unknown, to number of known age if age known
          missing == "sexAge" ~ NA_real_,
          missing == "age" ~ female_AgeUnknown,
          missing == "none" ~ female_AgeKnown
        )
  ) %>%
  mutate(male = case_when(
    missing == "sexAge" ~ NA_real_,
    missing == "age" ~ male_AgeUnknown,
    missing == "none" ~ male_AgeKnown
    )
  ) %>%
  select(year, origin_iso3, origin_country, asylum_iso3, asylum_country, popType, missing,
         female_0_4:female_60, female, male_0_4:male_60, male, total,
         origin_region, origin_subregion, origin_m49:origin_hcr_subregion,
         asylum_region, asylum_subregion, asylum_m49:asylum_hcr_subregion,
         neighbor:gni_asylum) %>%
  mutate(across(c(origin_iso3:missing, origin_region:asylum_hcr_subregion), factor)) %>%
  mutate(neighbor = factor(neighbor, levels = c("Yes", "No"))) %>%
  mutate(year = as.integer(year)) %>%
  group_by(asylum_iso3) %>%
  mutate(distanceAsyMeanW = weighted.mean(distance, w=total, na.rm=T)
  ) %>%
  ungroup() %>%
  mutate(distance = case_when(
    distance == 0 ~ distanceAsyMeanW,
    !(distance == 0 ) ~ distance
  )
  ) %>%
  mutate(gniRatio = gni_asylum/gni_origin,
         logGniRatio = log(gniRatio),
         logDistance = log(distance)
  ) %>%
  mutate(children = rowSums(select(., female_0_4, female_5_11, female_12_17,
                                   male_0_4, male_5_11, male_12_17)),
         adults = rowSums(select(., female_18_59, female_60,
                                 male_18_59, male_60))
  )

dem_longMissing <- dem_longMissing %>%
  mutate(asylum_sdgregion = case_when(
    asylum_subregion %in% c("Australia and New Zealand") ~ "Australia and New Zealand",
    asylum_subregion %in% c("Central Asia", "Southern Asia") ~ "Central and Southern Asia",
    asylum_subregion %in% c("Eastern Asia") ~ "Eastern Asia",
    asylum_subregion %in% c("South-eastern Asia") ~ "South-eastern Asia",
    asylum_subregion %in% c("Eastern Europe", "Southern Europe") ~ "Eastern and Southern Europe",
    asylum_subregion %in% c("Latin America and the Caribbean") ~ "Latin America and the Caribbean",
    asylum_subregion %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Oceania",
    asylum_subregion %in% c("Northern Africa", "Western Asia") ~ "Northern Africa and Western Asia",
    asylum_subregion %in% c("Northern America") ~ "Northern America",
    asylum_subregion %in% c("Northern Europe", "Western Europe") ~ "Northern and Western Europe",
    asylum_subregion %in% c("Sub-Saharan Africa") ~ "Sub-Saharan Africa"
    )
  )



##### VII. Final checks: internal consistency of dataset, totals and proportion of missingness #####

## internal consistency

# for age and sex available: do age categories add up to count in sex category?
t.ageSexCheck <- dem_longMissing %>%
  filter(missing == "none") %>%
  mutate(female_AgeKnown = rowSums(select(., female_0_4, female_5_11, female_12_17, female_18_59, female_60), na.rm = T),
         male_AgeKnown = rowSums(select(., male_0_4, male_5_11, male_12_17, male_18_59, male_60), na.rm = T)) %>%
  mutate(femaleAgeDiff = female - female_AgeKnown,
         maleAgeDiff = male - male_AgeKnown) %>%
  summarise(across(c(femaleAgeDiff, maleAgeDiff), ~summary(.))) # OK (all 0, age categories add up to sex category)

# for age and sex or only available: do male/female add up to total count?
t.sexTotalCheck <- dem_longMissing %>%
  filter(missing == "none" | missing == "age") %>%
  mutate(sexKnown = rowSums(select(., female, male), na.rm = T)) %>%
  mutate(sexDiff = total - sexKnown) %>%
  summarise(sexDiff = summary(sexDiff)) # OK (all 0, male/female add up to total)



## check headline totals against 2021 Global Trends and refugee data finder
t.total <- dem_longMissing %>% # ref finder:
  summarise(total = sum(total)) # OK

t.totalPoptype <- dem_longMissing %>% #
  group_by(popType) %>%
  summarise(total = sum(total)) # OK

t.totalRegion <- dem_longMissing %>% #
  group_by(popType, asylum_hcr_region) %>%
  summarise(total = sum(total)) # OK (rounding errors in GT table)


## check missingness proportion
t.misProp <- dem_longMissing %>%
  group_by(missing) %>%
  summarise(total = sum(total)) %>%
  mutate(prop = total/sum(total))

t.popType.misProp <- dem_longMissing %>% # GT 2021: "For example, demographic data by age and sex is available for 84 per cent of refugees and 42 per cent of Venezuelans displaced abroad at the end of 2021"
  group_by(popType, missing) %>%
  summarise(total = sum(total)) %>%
  mutate(prop = total/sum(total)) # OK, same as GT


##### VIII. Save dataset in data folder #####

save(dem_longMissing, m49hcr, file = "data/dem_refoip_end2022.RData")


############################################ END ###########################################################
