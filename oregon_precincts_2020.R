packages <- c("sf", "raster", "spData", "spDataLarge", "leaflet", "rgdal", "readr", "stringr", "RColorBrewer", "tmap", "ggplot2", "tmaptools", "tidyverse")
library(easypackages)
libraries(packages)

# Point addresses loading.  Use for an entry box in future app.

address_input <- c("2850 SW Fern St. Portland OR")

address_raw <- geocode_OSM(address_input)

address <- tibble(
  place=c(address_input),
  longitude=address_raw$coords["x"],
  latitude=address_raw$coords["y"])

address_sf <- st_as_sf(address, coords = c("longitude", "latitude"), 
                       crs = "NAD83", agr = "constant")


#Oregon county shapefiles.  Load and transform.

or_counties_sf <- st_read("shapefiles/oregon_counties/counties.shp") %>% 
  st_transform(or_counties, crs = "NAD83") 

#metro Portland precinct files

metro_portland_precinct_sf <- st_read("shapefiles/metro_portland_precinct/precinct.shp") %>% 
  select(PRECINCTID, COUNTY, geometry) %>%
  mutate(COUNTY = case_when(
    COUNTY == "W" ~ "Washington",
    COUNTY == "M" ~ "Multnomah",
    COUNTY == "C" ~ "Clackamas",
    TRUE ~ "Other"
    ) 
  ) %>% 
  st_transform(crs = "NAD83")


#include Washington for Clark County boundary

wa_counties_sf <- st_read("shapefiles/washington_counties/WA_County_Boundaries.shp") %>% st_transform(crs = "NAD83") %>% 
  filter(JURISDIC_2 == "Clark")

clark_wa_precincts_sf  <- st_read("clark_shapefiles/Precinct.shp") %>%
  select(PRECINCTID = PRECINCT, geometry) %>%
  mutate(PRECINCTID = str_c("K", as.character(PRECINCTID)), COUNTY = "Clark") %>% 
  st_transform(crs = "NAD83")

metro_portland_precinct_sf <- bind_rows(metro_portland_precinct_sf, clark_wa_precincts_sf)

or_counties_and_clark_sf <- bind_rows(or_counties_sf, wa_counties_sf)

#Load the PSU precinct shapefile for the rest of the state, excluding the three Portland Metro counties.

or_precinct_sf <- st_read("shapefiles/oregon_precincts/OregonPrecinctsNov2018.shp") %>% 
  filter(!County %in% c("Multnomah", "Washington", "Clackamas")) %>% 
  st_transform("NAD83")

#Oregon precinct votes from the OpenElections website.  Separates and excludes eight of 30 counties

separated_counties <- c("Multnomah", "Washington", "Clackamas", "Polk", "Yamhill", "Marion", "Tillamook", "Lane")

or_precincts_votes <- read_csv("data/votes/oregon_precincts.csv", col_types = cols(.default = "c", votes = "d")) %>%   mutate(party = case_when(
    str_detect(candidate,"Biden") ~ "DEM",
    str_detect(candidate,"Trump") ~ "REP",
    TRUE ~ party
  )
  ) %>% 
  filter(office == "President" & party %in% c("DEM","REP")) %>% 
  filter(!county %in% separated_counties) %>% 
  select(county, precinct, party, votes) %>% 
  pivot_wider(names_from = party, values_from = votes) 


#Lane County voter data

lane_votes <- read_csv("data/votes/lane.csv", col_types = cols(.default = "d", precinct = "c")) %>% 
  mutate(county = "Lane") %>% 
  rename(DEM = Biden, REP = Trump)

#Marion, Polk, and Yamhill County voter data.  Note these use the OpenElections standard format.  Portland Metro counties generated further down.  

open_election_col_types = cols(.default = "c", votes = "d")

marion_votes <- read_csv("data/votes/20201103__or__general__marion__precinct.csv", col_types = open_election_col_types) %>% 
  filter(office == "President") %>% 
  mutate(party = case_when(
    str_detect(candidate,"Biden") ~ "DEM",
    str_detect(candidate,"Trump") ~ "REP",
    TRUE ~ party
  ) ) %>% 
  filter(party %in% c("DEM", "REP")) %>% 
  select(!candidate) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  select(county, precinct, REP, DEM)

polk_votes <- read_csv("data/votes/20201103__or__general__polk__precinct.csv", col_types = open_election_col_types) %>% 
  filter(office == "President") %>% 
  mutate(party = case_when(
    str_detect(candidate,"Biden") ~ "DEM",
    str_detect(candidate,"Trump") ~ "REP",
    TRUE ~ party
  ) ) %>% 
  filter(party %in% c("DEM", "REP")) %>% 
  select(!candidate) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  select(county, precinct, REP, DEM)

tillamook_votes <- read_csv("data/votes/tillamook_votes.csv", col_types = cols(.default = "c", DEM = "d", REP = "d")) %>% 
  rename(precinct = precinct_votefile) %>% 
  select(county, precinct, DEM, REP)

yamhill_votes <- read_csv("data/votes/20201103__or__general__yamhill__precinct.csv", col_types = open_election_col_types) %>% 
  filter(office == "President") %>% 
  mutate(party = case_when(
    str_detect(candidate,"Biden") ~ "DEM",
    str_detect(candidate,"Trump") ~ "REP",
    TRUE ~ party
  ) ) %>% 
  filter(party %in% c("DEM", "REP")) %>% 
  select(!candidate) %>% 
  pivot_wider(names_from = party, values_from = votes) %>% 
  select(county, precinct, REP, DEM)

or_precincts_votes <- bind_rows(or_precincts_votes, tillamook_votes, lane_votes, polk_votes, marion_votes, yamhill_votes)


# read in a lookup table that matches vote file precinct names and shape file precinct names

lookup <- read_csv("data/votes_shapefile_lookup.csv", col_types = cols(.default = "c")) %>% 
  mutate(county = str_to_title(tolower(county))) 

# combine lookup table and shapefile

or_precinct_sf <- left_join(or_precinct_sf, lookup) %>% 
  select(!county) 

# need to trim whitespace.  Should reeplace with tidy version 

or_precinct_sf$precinct_votefile <- gsub('\\s+', '', or_precinct_sf$precinct_votefile)
or_precincts_votes$precinct <- gsub('\\s+', '', or_precincts_votes$precinct)

or_precinct_sf <- left_join(or_precinct_sf, or_precincts_votes, by = c("County" = "county","precinct_votefile" = "precinct" )) %>% 
  select(county = County, precinct = precinct_votefile, REP, DEM, geometry) %>% 
  st_transform(crs = "NAD83")






#generate metro_portland here

# work out the problematic Clackamas County precincts

clackamas_precincts_combined <- tribble(
  ~p1, ~p2,
  "007", "010",
  "070", "071",
  "099", "100",
  "103", "104",
  "251", "252",
  "361", "362",
  "417", "418"
) %>% 
  mutate(PRECINCTID = str_c("C",p1,"_",p2))

#this function deals with combining the relevant Clackamas precincts
combine_precincts <-function (df, insert_df) {
  
  insert_df <- insert_df %>% 
    mutate(p1 = str_c("C", p1),
           p2 = str_c("C", p2))

  extract <- df %>% filter(PRECINCTID %in% insert_df$p1 | 
                                   PRECINCTID %in% insert_df$p2 )
  
  remaining <- df %>% rows_delete(extract, by = "PRECINCTID")
  
  insert_df <- insert_df %>% pivot_longer( 
    cols = c(p1,p2), 
    names_to = "p", 
    values_to = "OLD_PRECINCTID") %>% 
    rename(NEWPRECINCTID = PRECINCTID, PRECINCTID = OLD_PRECINCTID) %>% 
    select(NEWPRECINCTID, PRECINCTID)
  
  extract <- inner_join(extract, insert_df, by = "PRECINCTID")
  extract <- extract %>% 
    select(-PRECINCTID) %>% 
    rename(PRECINCTID = NEWPRECINCTID)
  
  df <- remaining %>% rows_insert(extract, by = "PRECINCTID")
  return(df)
}

metro_portland_precinct_sf <- combine_precincts(metro_portland_precinct_sf, clackamas_precincts_combined)

multnomah_votes <- read_csv("data/votes/multnomah_votes.csv", 
                            col_types = cols(.default = "d", precinct = "c")) %>% 
  rename(Biden = biden, Trump = trump) %>% 
  relocate(precinct, Biden, Trump) %>% 
  mutate(county = "Multnomah", PRECINCTID = str_c("M", precinct))

clackamas_votes <- read_csv("data/votes/clackamas_votes.csv", col_types = cols(.default = "d", precinct = "c")) %>%
  mutate(county = "Clackamas", PRECINCTID = str_c("C", precinct))

washington_votes <- read_csv("data/votes/washington_county_votes.csv", col_types = cols(.default = "d", precinct = "c")) %>% 
  mutate(county = "Washington", PRECINCTID = str_c("W", precinct))

clark_votes <- read_csv("data/votes/clark_votes.csv", col_types = cols(.default = "d", precinct = "c")) %>% 
  mutate(county = "Clark", PRECINCTID = str_c("K", precinct))

metro_portland_votes <- bind_rows(multnomah_votes, clark_votes, washington_votes, clackamas_votes) %>% select(-other)

metro_portland_votes <- metro_portland_votes %>% 
  mutate(vpct = Biden/(Biden + Trump)) %>% 
  mutate(pwinner = ifelse(vpct >=.5, "Biden", "Trump")) %>% 
  mutate(pct_lbl = ifelse(pwinner == "Biden", str_c(pwinner, " ", format(vpct*100, digits = 3)), str_c(pwinner, " ", format(100 - vpct*100, digits = 2)))) %>% 
  mutate(hover = str_c(precinct, ": ", pct_lbl)) %>% 
  dplyr::select(PRECINCTID, precinct, county, vpct, Biden, Trump, hover)

metro_portland_precinct_sf <- inner_join(metro_portland_precinct_sf, metro_portland_votes, by=("PRECINCTID")) %>% 
  select(county = COUNTY, precinct, DEM = Biden, REP = Trump)

or_precinct_sf <- bind_rows(or_precinct_sf, metro_portland_precinct_sf)

or_precinct_sf <- or_precinct_sf %>% 
  mutate(vpct = DEM/(DEM + REP)) %>% 
  mutate(pwinner = ifelse(vpct >=.5, "DEM", "REP"), DPR = DEM + REP) %>% 
  mutate(pct_lbl = ifelse(pwinner == "DEM", str_c(pwinner, " ", format(vpct*100, digits = 3)), str_c(pwinner, " ", format(100 - vpct*100, digits = 2)))) %>% 
  mutate(hover = str_c(county," ",precinct, ": ", pct_lbl," ",DPR)) %>% 
  dplyr::select(county, precinct, vpct, DEM, REP, hover)

rwb <- colorRampPalette(c("dark red", "white", "dark blue"))(256)

tmap_mode("view")

pctiles <- c(0,.1,.3,.45,.5,.55,.7,.9,1)
bb_or <- bb(metro_portland_precinct_sf)

tmap_options(basemaps = c(Canvas = "OpenStreetMap"))

or_precincts_tmap <- 
  tm_shape(or_precinct_sf, bbox = bb_or) + 
  tm_polygons(col = "vpct", n=length(5), id = "hover", style = "fixed", breaks = pctiles, alpha = 0.7, palette = rwb) +
  tm_shape(or_counties_and_clark_sf) + 
  tm_borders(lwd = 1, col = "green") +
  tm_shape(address_sf) + 
  tm_dots()

or_precincts_tmap

