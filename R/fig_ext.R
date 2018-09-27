# packages
library(tidyverse)
library(gridExtra)

# data
data(ascidat)
data(suppdat)

# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# env vars to eval
evvar <- c('Phosphorus_as_P_mgPerL', 'Nitrogen_Total_mgPerL', 'XCDENMID', 'PCT_SAFN', 'SpecificConductivity_uSPercm', 'Temperature_Deg_C') 
names(evvar) <- c('Phosphorus (mg/L)', 'Total Nitrogen (mg/L)', '% Shading', '% Sands and Fines', 'Conductivity (mS/cm)', 'Temperature (C)')

# base theme
pbase <- theme_minimal(base_family = 'serif', base_size = 15) +
  theme(
    panel.border = element_rect(colour = 'black', fill = NA, size = 0.5),
    legend.position = 'top', 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank()
  )

# csci by rc
tonrm <- suppdat %>% 
  select(scr, sit1) %>% 
  filter(sit1 %in% 'rc') %>% 
  pull(scr) %>% 
  mean(na.rm = T)

##
# first plot

cscidat <- suppdat %>% 
  filter(ind %in% 'CSCI') %>% 
  select(sampleid, ind, scr, Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, PCT_SAFN, SpecificConductivity_uSPercm) %>% 
  spread(ind, scr)
toplo <- ascidat %>% 
  select(sampleid, ind, tax, scr) %>% 
  filter(ind %in% 'MMI') %>% 
  unite(ind, c('ind', 'tax')) %>% 
  spread(ind, scr) %>% 
  inner_join(cscidat, by = 'sampleid') %>% 
  gather('ind', 'scr', MMI_Diatom, MMI_Hybrid, `MMI_Soft-bodied`, CSCI) %>% 
  mutate(ind = case_when(
                         ind == 'MMI_Diatom' ~ 'ASCI_D',
                         ind == 'MMI_Hybrid' ~ 'ASCI_H',
                         ind == 'MMI_Soft-bodied' ~ 'ASCI_S',
                         ind == 'CSCI' ~ 'CSCI'
                         )) %>% 
  gather('var', 'val', Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, PCT_SAFN, SpecificConductivity_uSPercm) %>% 
  mutate(
    var = factor(var, levels = evvar, labels = names(evvar)),
    var = as.character(var)
  )

# colors
cols <- RColorBrewer::brewer.pal(9, 'Set1')[1:4]

# plots
plos <- toplo %>% 
  group_by(var) %>% 
  nest %>% 
  mutate(
    plos = pmap(list(var, data), function(var, data){
      
      p <- ggplot(data, aes(x = val, y = scr, group = ind, colour = ind)) + 
        geom_smooth(se = F) +
        xlab(var) + 
        # scale_y_continuous(limits = c(0.5, 1.05)) +
        scale_colour_manual(values = cols) +
        theme_bw() +
        theme(
          axis.title.y = element_blank(),  
          legend.position = 'none'
        )
      
      if(var %in% c("Phosphorus (mg/L)", "Total Nitrogen (mg/L)"))
        p <- p + scale_x_log10(var)
        
      return(p)
      
    })
  )

p1 <- plos$plos[[1]]
p2 <- plos$plos[[2]]
p3 <- plos$plos[[3]]
p4 <- plos$plos[[4]]

pleg <- ggplot(toplo, aes(x = val, y = scr, group = ind, colour = ind)) + 
  geom_smooth(se = F) + 
  theme_bw() + 
  theme(legend.title = element_blank(), legend.position = 'top') +
  scale_colour_manual(values = cols)
pleg <- g_legend(pleg)

pdf('C:/Users/Marcus.SCCWRP2K/Desktop/fig1.pdf', height = 6, width = 6, family = 'serif')
grid.arrange(
  arrangeGrob(
    pleg, 
    arrangeGrob(p1, p2, p3, p4, ncol = 2, left = 'Index score'), 
    ncol = 1, heights = c(0.1, 1)
  )
)
dev.off()

##
# second plot

toplo <- ascidat %>% 
  select(sampleid, ind, tax, scr) %>% 
  filter(ind %in% 'MMI' & tax %in% 'Hybrid') %>% 
  unite(ind, c('ind', 'tax')) %>% 
  spread(ind, scr) %>%
  inner_join(cscidat, by = 'sampleid') %>% 
  gather('var', 'val', Nitrogen_Total_mgPerL, Phosphorus_as_P_mgPerL, PCT_SAFN, SpecificConductivity_uSPercm) %>% 
  mutate(
    var = factor(var, levels = evvar, labels = names(evvar)),
    var = as.character(var), 
    CSCI = CSCI / tonrm, 
    scrdif = CSCI - MMI_Hybrid
  )

# plots
plos <- toplo %>% 
  group_by(var) %>% 
  nest %>% 
  mutate(
    plos = pmap(list(var, data), function(var, data){
      
      p <- ggplot(data, aes(x = val, y = scrdif)) + 
        geom_point() + 
        geom_smooth(method = 'lm', se = F) +
        xlab(var) + 
        # scale_y_continuous(limits = c(0.5, 1.05)) +
        scale_colour_manual(values = cols) +
        theme_bw() +
        theme(
          axis.title.y = element_blank()
        ) + 
        geom_hline(yintercept = 0,linetype = 'dashed')
      
      if(var %in% c("Phosphorus (mg/L)", "Total Nitrogen (mg/L)"))
        p <- p + scale_x_log10(var)
      
      return(p)
      
    })
  )

p1 <- plos$plos[[1]]
p2 <- plos$plos[[2]]
p3 <- plos$plos[[3]]
p4 <- plos$plos[[4]]

pdf('C:/Users/Marcus.SCCWRP2K/Desktop/fig2.pdf', height = 5, width = 6, family = 'serif')
grid.arrange(
  p1, p2, p3, p4, ncol = 2, left = 'CSCI - ASCI_H' 
)
dev.off()

