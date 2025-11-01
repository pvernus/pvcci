### ###
## Population count
### ###

# import data
# source: European Commission. GHSL Data Package 2023.
r_pop_GHS_1990_2022_5arcmin <- terra::rast("C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\downscaled_gridded_gdp\\code_input_data\\data_gis\\r_pop_GHS_1990_2022_5arcmin.tif")

# extract population (raster) data from adm2 (polygons)
pop_adm2 <- exactextractr::exact_extract(r_pop_GHS_1990_2022_5arcmin, adm2_gadm_clean, 
                                         fun = 'sum', 
                                         append_cols = c("iso3", "gid_adm2", "name_adm2")) |> 
  pivot_longer(
    cols = starts_with("sum.pop"),  
    names_to = "year",
    values_to = "pop"   
  ) |> 
  mutate(
    year = str_extract(year, "\\d{4}") |> 
      as.numeric()
  )

### ###
## downscaled GDP (per capita)
### ###

polyg_adm2_gdp_perCapita_1990_2022 <- sf::read_sf(
  "C:\\Users\\pauvernu\\Seafile\\library\\chap_two_data\\downscaled_gridded_gdp\\polyg_adm2_gdp_perCapita_1990_2022.gpkg")
gdppc_adm2 <- polyg_adm2_gdp_perCapita_1990_2022 |> 
  clean_names() |> 
  pivot_longer(
    cols = starts_with("x"),  
    names_to = "year",
    values_to = "gdppc"   
  ) |> 
  mutate(
    year = str_extract(year, "\\d{4}") |> 
      as.numeric()
  ) |> 
  select(gid_adm2 = gid_2, name_adm2 = name_2, year, iso3, gdppc, slope)

### ###
## Conflict UCDP
### ###

GEDEvent_v25_0_8 <- read_csv("C:/Users/pauvernu/Downloads/GEDEvent_v25_0_8.csv")

exactextractr::exact_extract(r_pop_GHS_1990_2022_5arcmin, adm2_gadm_clean, 
                             fun = 'sum', 
                             append_cols = c("iso3", "gid_adm2", "name_adm2"))