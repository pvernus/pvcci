### create adm2 polygons units
## adapted from: https://github.com/mattikummu/griddedGDPpc/blob/main/5_downscaling_predict.R
  
#pckg
library(sf)
library(readr)
library(janitor)
library(stringi)
library(tidyverse)
library(countrycode)

# load data
# adm2_gadm_clean <- st_read('C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\gadm_410-gpkg\\adm2_gadm_clean.gpkg')

## load gadm source data
# adm0
adm0_gadm <- read_sf('C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\gadm_410-gpkg\\gadm_410-levels.gpkg', layer ='ADM_0') |> 
  clean_names() |> 
  mutate(gid_0 = ifelse(gid_0 == 'XKO', 'XKX', gid_0)) |> 
  # filter(!gid_0 == 'ALA') |> # remove åland, as part of finland
  rename(iso3 = gid_0)

# adm1
adm1_gadm <- read_sf('C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\gadm_410-gpkg\\gadm_410-levels.gpkg', layer ='ADM_1') |> 
  clean_names() |> 
  mutate(gid_0 = ifelse(gid_0 == 'XKO', 'XKX', gid_0)) |> 
  # filter(!gid_0 == 'ALA') |> # remove åland, as part of finland
  rename(iso3 = gid_0)

#adm2
adm2_gadm <- read_sf('C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\gadm_410-gpkg\\gadm_410-levels.gpkg', layer ='ADM_2') |> 
  clean_names() |> 
  mutate(gid_0 = ifelse(gid_0 == 'XKO', 'XKX', gid_0)) |> 
  # filter(!gid_0 == 'ALA') |> # remove åland, as part of finland
  rename(iso3 = gid_0)

# missing adm data
# adm1_gadm |> filter(!iso3 %in% unique(adm2_gadm$iso3))
# adm0_gadm |> filter(!iso3 %in% c(unique(adm2_gadm$iso3), unique(adm1_gadm$iso3)))

adm2_gadm_clean <- adm2_gadm |>
  # if not adm2 data, use adm1 data
  bind_rows(adm1_gadm |> filter(!iso3 %in% unique(adm2_gadm$iso3))) |>
  # finally, if not in either adm2 or adm1, use adm0
  bind_rows(adm0_gadm |> filter(!iso3 %in% c(unique(adm2_gadm$iso3), unique(adm1_gadm$iso3)))) |>
  mutate(across(c(name_1, name_2), na_if, "?")) |> 
  mutate(name_adm2 = ifelse(is.na(name_2) & is.na(name_1), country,
                         ifelse(is.na(name_2), name_1, name_2) )) |>
  mutate(gid_adm2 = ifelse(is.na(gid_2) & is.na(gid_1), iso3,
                        ifelse(is.na(gid_2), gid_1, gid_2))) |>
  select(country, iso3, gid_adm1 = gid_1, name_adm1 = name_1, gid_adm2, name_adm2) |> 
  # some of the adm1 levels are divided to those that are officially in a country and those that are
  # on conflict zones (between CHN, IND and PAK)
  # let's rename them with those that we have data for
  mutate(iso3 = ifelse(iso3 %in% c('Z02', 'Z03', 'Z08'),'CHN',
                       ifelse(iso3 %in% c('Z06'), 'PAK',
                              ifelse(iso3 %in% c('Z01', 'Z04','Z05','Z07','Z09'), 'IND',
                                     iso3))))

st_write(adm2_gadm_clean, 
         'C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\gadm_410-gpkg\\adm2_gadm_clean.gpkg', delete_dsn=T)

# NOTE: Macao and Hong Kong are misssing in GADM .gpkg file, not sure why

### ###
## PVCCI
### ###

adm2_gadm_v3.6 <- st_read("C:/Users/pauvernu/Seafile/library/chap_two_data/gadm_adm2p/gadm_adm2p.shp") |>
  clean_names()

# Download from https://gadm.org/data.html failed
# usethis::use_zip(
# url = "https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_gpkg.zip",
# destdir = "C:/Users/pauvernu/Seafile/library/chap_two_data/gadm_adm2p"
# )

# read data
pvcci_subnational <- readxl::read_excel("C:/Users/pauvernu/Seafile/library/chap_two_data/20251008_pvcci_os.xlsx") |> 
  clean_names() |> 
  mutate(across(c(name_0, name_1, name_2), \(x) na_if(x, "0"))) |> 
  mutate(iso = case_when( # iso w/ different iso code in gadm
    iso == "XKO" ~ "XKX",
    iso == "XNC" ~ "ZNC",
    .default = iso
  )) |>   
  filter(!iso %in% c("SP-", "PIS")) |> # iso w/ no value (probably typos)
  rename(iso3 = iso, name_adm0 = name_0, name_adm1 = name_1, name_adm2 = name_2) |> 
  as_tibble()

# create new variables (agg w/ quadratic mean)
pvcci <- pvcci_subnational |> 
  mutate(
    fast_onset_pvcci = sqrt((flood_norm^2 + aridity_norm^2) / 2),
    slow_onset_pvcci = sqrt((rainfall_norm^2 + temperature_norm^2 + storms_norm^2) / 3)
  )

####

adm_gadm_list <- adm2_gadm_clean |> 
  st_set_geometry(NULL) |> 
  mutate(across(starts_with("name_adm"), 
                \(x) make_clean_names(x, 
                                      allow_dupes = TRUE),
                .names = "{.col}_clean")) |> 
  distinct(iso3, gid_adm1, name_adm1, name_adm1_clean, gid_adm2, name_adm2, name_adm2_clean)

adm_pvcci_list <- pvcci |> 
  select(iso3, starts_with("name_adm"), pvcci) |> 
  mutate(across(starts_with("name_adm"), 
                \(x) make_clean_names(x, allow_dupes = TRUE),
                .names = "{.col}_clean")) |> 
  distinct(iso3, name_adm1_clean, name_adm2_clean, pvcci)

# merge on iso3, name_adm1_clean, name_adm2_clean
exact_merged <- left_join(adm_gadm_list, adm_pvcci_list,
          by = c("iso3", "name_adm1_clean", "name_adm2_clean"),
          suffix = c("_gadm", "_pvcci")) |> 
  View()

  filter(is.na(name_adm2_clean))


# 29,242 / 48,010 units matched
# 18,768 / 48,010 units unmatched

anti_merged <- exact_merged |> 
  filter(!is.na(name_adm2_pvcci))

missing_merged <- adm_gadm_list |> 
  filter(!name_adm2_clean %in% anti_merged$name_adm2_clean)

# adm0
anti_join(
  distinct(missing_merged, iso3),
  distinct(adm_pvcci_list, iso3),
  by = "iso3"
)

# adm1
anti_join(
  distinct(missing_merged, iso3, name_adm1_clean),
  distinct(adm_pvcci_list, iso3, name_adm1_clean),
  by = c("iso3", "name_adm1_clean")
) |> 
  View()

# adm2
anti_join(
  distinct(missing_merged, iso3, name_adm1_clean, name_adm2_clean),
  distinct(adm_pvcci_list, iso3, name_adm1_clean, name_adm2_clean),
  by = c("iso3", "name_adm1_clean", "name_adm2_clean")
)
  
# iso: missing in pvcci, different code
# check unique iso in each
# check anti_join, in adm_pvcci_list but missing in merged_iso

# adm1: missing in pvcci, different spelling
## missing: match on adm2, then group by adm1 and sum matches, non-null means existing
## spelling: fuzzyjoin, IA, manual
## check unique iso, adm1 in each
# adm2: missing in pvcci, different spelling
## missing: match w/ adm1
## spelling: 

####

# first: merge adm0
merged_adm0 <- left_join(
  adm2_gadm_clean |> 
    st_set_geometry(NULL) |> 
    distinct(iso3),
  pvcci |> 
    distinct(iso3),
  by = "iso3"
)

adm2_gadm_clean |> 
  st_set_geometry(NULL) |> 
  distinct(iso3) |> 
  filter(!iso3 %in% adm0_pvcci_merged$iso3)

### adm2_gadm_clean
## does not exist in PVCCI 
# Antarctica: ATA
# Caspian Sea: XCA
# Paracel Islands: XPI
# Spratly Islands: XSP

pvcci |> 
  distinct(iso3) |> 
  filter(!iso3 %in% adm0_pvcci_merged$iso3)

### PVCCI
## does not exist in adm2_gadm_clean (why?)
# Macao: MAC
# Hong Kong: HKG

# second: merge adm1

# list unique name_adm1 in gadm and clean names
adm1_gadm_list <- adm2_gadm_clean |> 
  st_set_geometry(NULL) |> 
  filter(!is.na(name_adm1)) |> 
  distinct(iso3, name_adm1, gid_adm1) |> 
  mutate(name_adm1_clean = make_clean_names(name_adm1))

# check for duplicates in gadm
get_dupes(adm1_gadm_list, name_adm1_clean)

# list unique name_adm1 in pvcci and clean names
adm1_pvcci_list <- pvcci |> 
  filter(!is.na(name_adm1)) |> 
  distinct(iso3, name_adm1) |> 
  mutate(name_adm1_clean = make_clean_names(name_adm1))

# check for duplicates in pvcci
get_dupes(adm1_pvcci_list, name_adm1_clean)

# left join name_adm1 gadm <- pvcci
merged_adm1_list <- fuzzyjoin::stringdist_left_join(
  adm1_gadm_list,
  adm1_pvcci_list,
  by = c(name_adm1_clean = "name_adm1_clean"),
  max_dist = 1,
  distance_col = "distance"
)

# some name_adm1 in pvcci are not matched to name_adm1 in gadm
unmatched_adm1_list <- adm1_pvcci_list |> 
  # only keep obs. with no matched adm1_name in gadm
  filter(!name_adm1_clean %in% merged_adm1_list$name_adm1_clean.x)
    
# some name_adm1 exist in both datasets but have different spelling
# ex: BLR Homyel' <--> Gomel
# Differences are usually too important for a fuzzyjoin
# However, such adm1 should match on their adm2

# list unique name_adm2 in gadm and clean names
adm2_gadm_list <- adm2_gadm_clean |> 
  st_set_geometry(NULL) |> 
  filter(!is.na(name_adm1), !is.na(name_adm2)) |> 
  distinct(iso3, name_adm1, gid_adm1, name_adm2, gid_adm2) |> 
  mutate(across(starts_with("name_adm"), 
                \(x) make_clean_names(x),
                .names = "{.col}_clean"))

# list unique name_adm1 and name_adm2 in pvcci and clean names
adm2_pvcci_list <- pvcci |> 
  filter(!is.na(name_adm1), !is.na(name_adm2)) |> 
  distinct(iso3, name_adm1, name_adm2) |> 
  mutate(across(starts_with("name_adm"), 
                \(x) make_clean_names(x),
                .names = "{.col}_clean"))

# left join name_adm2 in gadm w/ adm2 in pvcci, only in unmatched adm1
# here, fuzzyjoin does not work well. Many false positives (i.e., matched name_adm2 that should not be)
# use left_join by = c("iso3", "name_adm2_clean") instead
merged_adm2_list <- left_join(
  adm2_pvcci_list |> 
    # only keep name_adm1 w/ unmatched name_adm1 in gadm
    filter(name_adm1_clean %in% unmatched_adm1_list$name_adm1_clean),
  adm2_gadm_list,
  by = c("iso3", "name_adm2_clean")
)

# out of the 200 unique name_adm2 w/ unmatched name_adm1, 118 match on c(iso, name_adm2)
# among the remaining 82 name_adm2, some in pvcci match w/ name_adm1 in gadm

merged_adm2_list |> 
  filter(is.na(name_adm2.y)) |> 
  View()

adm2_gadm_list |> 
  View()

left_join(
  merged_adm2_list |> 
    filter(is.na(name_adm2.y)),
  adm2_gadm_list,
            by = c("iso3", "name_adm1_clean.x" = "name_adm2_clean")) |> 
  View()
