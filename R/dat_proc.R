library(tidyverse)
library(lubridate)
library(sf)

######
# september 2018 figs for Susie
# data from https://drive.google.com/open?id=19PLgB1zcBTEa76B1kTFu6AYzNDuOx51q

data(psa)
prj <- '+proj=longlat +datum=WGS84 +no_defs'

# asci scores, get scores and site types only
asciall <- read.csv('raw/asci.scores.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(X, ASCI.diatom, MMI.diatom, OoverE.diatom, Type.diatom, Type.diatom.1, ASCI.sba, MMI.sba, OoverE.sba, Type.sba, Type.sba.1, ASCI.hybrid, MMI.hybrid, OoverE.hybrid, Type.hybrid, Type.hybrid.1) %>% 
  rename(
    sampleid = X
  ) %>% 
  rename_all(
    funs(
      tolower(.) %>%
        gsub('\\.', '_', .)
    )
  ) 

# site types1 1 only, long format
ascisit1 <- asciall %>% 
  dplyr::select(matches('sampleid|^type')) %>% 
  dplyr::select(-matches("_1$")) %>% 
  gather('tax', 'sit1', -sampleid) %>% 
  mutate(
    tax = gsub('^type_', '', tax)
  )

# site types 2 only, long format
ascisit2 <- asciall %>% 
  dplyr::select(matches('sampleid|_1$')) %>% 
  gather('tax', 'sit2', -sampleid) %>% 
  mutate(
    tax = gsub('^type_|_1$', '', tax)
  )

# site scores only, long format
asciind <- asciall %>% 
  dplyr::select(matches('sampleid|^asci|^mmi|^oovere')) %>% 
  gather('typ', 'scr', -sampleid) %>% 
  separate(typ, c('ind', 'tax'), sep = '_')

# join the two long format
ascidat <- asciind %>% 
  left_join(ascisit1, by = c('sampleid', 'tax')) %>% 
  left_join(ascisit2, by = c('sampleid', 'tax')) %>% 
  mutate(
    site = gsub('^(.*)_.*_.*$', '\\1', sampleid),
    date = gsub('^.*_(.*)_.*$', '\\1', sampleid), 
    smps = gsub('^.*_.*_(.*)$', '\\1', sampleid), 
    date = lubridate::mdy(date), 
    tax = factor(tax, levels = c('diatom', 'sba', 'hybrid'), labels = c('Diatom', 'Soft-bodied', 'Hybrid')), 
    ind = factor(ind, levels = c('oovere', 'mmi', 'asci'), labels = c('O/E', 'pMMI', 'ASCI')),
    sit2 = factor(sit2, levels = c('Reference', 'Intermediate', 'Stressed'))
    )

##
# ref percentiles
ascithr <- ascidat %>% 
  filter(sit1 %in% 'rc') %>% 
  dplyr::select(sampleid, tax, ind, scr) %>% 
  crossing(ptile = c(0.01, 0.1, 0.3)) %>% 
  group_by(tax, ind, ptile) %>% 
  nest %>% 
  mutate(
    thrsh = purrr::pmap(list(data, ptile), function(data, ptile) quantile(x = data$scr, probs = ptile, na.rm = T))
  ) %>% 
  dplyr::select(-data) %>% 
  unnest

##
# csci, ibi, and supporting data

# csci scores
csciall <- read.csv('raw/csci.scores.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(SampleID_old, CSCI) %>% 
  rename(
    sampleid = SampleID_old, 
    csci = CSCI
  )

# algae IBIs
ibisall <- read.csv('raw/ibi.scores.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(SampleID_old, S2, D18, H20) %>% 
  rename(
    sampleid = SampleID_old
  )

# site data  
siteall <- read.csv('raw/algae.site.data.csv', stringsAsFactors = FALSE) %>% 
  dplyr::select(X, StationCode, New_Lat, New_Long, Phosphorus_as_P_mgPerL, Nitrogen_Total_mgPerL, SpecificConductivity_uSPercm, XCDENMID, PCT_SAFN, Temperature_Deg_C) %>% 
  rename(
    sampleid = X, 
    lat = New_Lat, 
    lon = New_Long
  )

# combine all suppdat
suppdat <- siteall %>% 
  full_join(csciall, by = 'sampleid') %>% 
  full_join(ibisall, by = 'sampleid')

# get psa by sampleid
psadat <- suppdat %>% 
  dplyr::select(sampleid, lat, lon) %>% 
  unique %>% 
  filter(!(is.na(lat)|is.na(lon))) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = prj) %>% 
  st_intersection(psa) %>% 
  dplyr::select(sampleid, PSA6)
st_geometry(psadat) <- NULL

# join psa to suppdat
suppdat <- suppdat %>% 
  left_join(psadat, by = 'sampleid') 

# filter any where all columns are na except sampleid
suppdat <- suppdat %>% 
  gather('var', 'val', -sampleid) %>% 
  filter(!is.na(val)) %>% 
  spread(var, val)

save(ascidat, file = 'data/ascidat.RData', compress = 'xz')
save(ascithr, file = 'data/ascithr.RData', compress = 'xz')
save(suppdat, file = 'data/suppdat.RData', compress = 'xz')