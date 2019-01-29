# compare CSCI/ASCI and third variable to color aesthetic

library(tidyverse)

data(suppdat)
data(ascidat)

# combine 
dat <- suppdat %>% 
  filter(ind %in% 'CSCI') %>% 
  spread(ind, scr) %>% 
  full_join(ascidat, by = 'sampleid') %>% 
  filter(ind %in% 'MMI') %>% 
  filter(tax %in% 'Hybrid') %>% 
  spread(ind, scr) %>% 
  rename(
    ASCI = MMI
  ) %>% 
  select(-sit1.x, -sit1.y, -sit2, -site, -date, -smps, -PSA6, -lon, -lat, -tax) %>% 
  rename(
    TN = Nitrogen_Total_mgPerL, 
    Cond = SpecificConductivity_uSPercm, 
    TP = Phosphorus_as_P_mgPerL,
    Temp = Temperature_Deg_C
  ) %>% 
  gather('var', 'val', -sampleid, -CSCI, -ASCI)

vrs <- dat %>% 
  pull(var) %>% 
  unique

pdf('C:/Users/Marcus.SCCWRP2K/Desktop/stress.pdf', height = 6, width = 6, family = 'serif')
for(vr in vrs){
  
  toplo <- dat %>% 
    filter(var %in% vr) %>% 
    na.omit
  
  p <- ggplot(toplo, aes(x = ASCI, y = CSCI, colour = val)) + 
    geom_abline(slope = 1, intercept = 0, linetype = 'dotted') +
    geom_point(alpha = 0.7, size = 2) + 
    geom_smooth(method = 'lm', se = F) + 
    theme(legend.position = 'top') + 
    scale_y_continuous(limits = c(0.1, 1.3)) +
    scale_x_continuous(limits = c(0.1, 1.3))
  
  if(vr %in% c('TP', 'TN'))
    p <- p + scale_color_distiller(vr, palette = 'RdYlGn', trans = 'log10')
  else  
    p <- p + scale_color_distiller(vr, palette = 'RdYlGn') 
  
  print(p)
  
}
dev.off()
  