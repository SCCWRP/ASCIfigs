library(tidyverse)
library(lubridate)
library(sf)

data(psa)
prj <- '+proj=longlat +datum=WGS84 +no_defs'

######
# september 2018 figs for Susie
# data from https://drive.google.com/open?id=19PLgB1zcBTEa76B1kTFu6AYzNDuOx51q

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
    ind = factor(ind, levels = c('oovere', 'mmi', 'asci'), labels = c('O/E', 'MMI', 'O/E + MMI')),
    sit2 = factor(sit2, levels = c('Reference', 'Intermediate', 'Stressed'))
    )

##
# ref percentiles
ascithr <- read.csv('raw/BIthresholds.csv', header = T) %>% 
  filter(!Index %in% 'CSCI') %>% 
  separate(Index, c('ind', 'tax'), sep = '_') %>% 
  mutate(
    tax = factor(tax, levels = c('D', 'S', 'H'), labels = c('Diatom', 'Soft-bodied', 'Hybrid'))
  ) %>% 
  select(-ind) %>% 
  gather('bigls', 'bival', -tax)

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
  dplyr::select(X, StationCode, New_Lat, New_Long, Phosphorus_as_P_mgPerL, Nitrogen_Total_mgPerL, SpecificConductivity_uSPercm, XCDENMID, PCT_SAFN, Temperature_Deg_C, XSLOPE, LogWSA, Elevation) %>% 
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
  left_join(psadat, by = 'sampleid') %>% 
  mutate_if(is.factor, as.character) 

# filter any where all columns are na except sampleid
suppdat <- suppdat %>% 
  gather('var', 'val', -sampleid, -StationCode, -PSA6) %>% 
  filter(!is.na(val)) %>% 
  spread(var, val)

# add psa data, lat, lon from suppdat to ascidat
ascidat <- suppdat %>% 
  select(sampleid, PSA6, lon, lat) %>% 
  unique %>% 
  na.omit %>% 
  left_join(ascidat, ., by = 'sampleid')

# get only relevant variables for plotting
# put old index values in long format
suppdat <- suppdat %>% 
  select(-lat, -lon, -PSA6, -StationCode) %>% 
  gather('ind', 'scr', csci, D18, S2, H20) %>% 
  mutate(
    ind = factor(ind, levels = c('csci', 'D18', 'S2', 'H20'), labels = c('CSCI', 'D18', 'S2', 'H20'))
  )

# get median values of algal indices at ref cal sites
algthrsh <- suppdat %>% 
  rename(
    algind = ind,
    algscr = scr
    ) %>% 
  left_join(ascidat, by = 'sampleid') %>% 
  select(sampleid, algind, algscr, sit1) %>% 
  unique %>% 
  filter(!algind %in% 'CSCI') %>% 
  filter(sit1 %in% 'rc') %>% 
  group_by(algind) %>% 
  summarise(
    thrsh = median(algscr, na.rm = T)
  ) %>% 
  ungroup %>% 
  rename(ind = algind)

# join algthrsh with suppdat to standardize
suppdat <- suppdat %>% 
  left_join(algthrsh, by = 'ind') %>% 
  mutate(
    scr = case_when(
      ind %in% 'CSCI' ~ scr, 
      TRUE ~ scr / thrsh
      
    )
  ) %>% 
  dplyr::select(-thrsh)
  
# get site types from ascidat to join with suppdat
suppdat <- ascidat %>% 
  select(sampleid, sit1) %>% 
  unique %>% 
  na.omit %>% 
  left_join(suppdat, ., by = 'sampleid')

save(ascidat, file = 'data/ascidat.RData', compress = 'xz')
save(ascithr, file = 'data/ascithr.RData', compress = 'xz')
save(suppdat, file = 'data/suppdat.RData', compress = 'xz')
