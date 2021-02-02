
# ADD COUNTY COLUMN TO EACH WELL
counties = USAboundaries::us_counties() %>%
  filter(state_name == "Arizona")
az_spatial = az_time %>%
  group_by(wellid) %>%
  arrange(desc(date)) %>% 
  slice(n = 1) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>% 
  na.omit() %>% 
  select(wellid, date, dtw)

az_spatial = st_join(az_spatial, counties)
az_time = left_join(az_time, select(az_spatial, wellid, name), by = "wellid") %>%
  select(!geometry)
az_time <- az_time %>% rename(county = name)


# READ IN USGS WATER WITHDRAWALS DATA
# SET UP LONG DATA FRAME OF ARIZONA COUNTIES, WITH SURFACE + GROUND WATER WITHDRAWALS FOR 5 MAIN WATER USERS
water_use <-  readxl::read_xlsx('data/usco2015v2.0.xlsx', 1, skip = 1) %>%
  janitor::clean_names() %>%
  rename(population = tp_tot_pop)
dict <-  readxl::read_xlsx('data/usco2015v2.0.xlsx', 2) 
# convert chr columns to numerics
water_use <- water_use %>%
  mutate(across(c(8:141), as.numeric))

withdrawal_total = water_use %>% 
  select(state, county, do_wsw_fr, do_wgw_fr,
         contains('wsw_to'), contains('wgw_to'),
         ir_wsw_fr, ir_wgw_fr,
         li_wsw_fr, li_wgw_fr)
withdrawal_total <- withdrawal_total %>%
  filter(state == "AZ") %>% 
  group_by(county) %>% 
  mutate(dom_surface = ps_wsw_to + do_wsw_fr,
         dom_groundwater = ps_wgw_to + do_wgw_fr,
         industrial_surface = in_wsw_to,
         industrial_groundwater = in_wgw_to,
         agr_surface = ir_wsw_fr + li_wsw_fr + aq_wsw_to,
         agr_groundwater = ir_wgw_fr + li_wgw_fr + aq_wgw_to,
         mining_surface = mi_wsw_to,
         mining_groundwater = mi_wgw_to,
         thermo_surface = pt_wsw_to,
         thermo_groundwater = pt_wgw_to) %>% 
  select(1:2, 25:34)
sector_total <- withdrawal_total %>%
  pivot_longer(cols = c(3:12), names_to = "sector", values_to = "withdrawal")

sector_total$source <- ifelse(grepl("surface", sector_total$sector, ignore.case = T), "Surface water", 
                               ifelse(grepl("groundwater", sector_total$sector, ignore.case = T), "Groundwater", "Other"))
sector_total$sector <- ifelse(grepl("industrial", sector_total$sector, ignore.case = T), "Industrial", 
                               ifelse(grepl("dom", sector_total$sector, ignore.case = T), "Domestic",
                                      ifelse(grepl("agr", sector_total$sector, ignore.case = T), "Agriculture",
                                             ifelse(grepl("mining", sector_total$sector, ignore.case = T), "Mining",
                                                    ifelse(grepl("thermo", sector_total$sector, ignore.case = T), "Thermoelectric", "Other"))))) 
sector_total <- sector_total %>% 
  select(state, county, sector, source, withdrawal)
sector_total$county = gsub(" County", '', sector_total$county)

# saveRDS(sector_total, "data/sector_total.rds")


geom_col(position = "fill", col = "black", alpha = 0.7)  +
  scale_y_continuous(labels = scales::percent) 









