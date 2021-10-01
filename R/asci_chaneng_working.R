# script to:
# 1) add data for channel type and flow status to asci dataset
# 2) explore additional visualizations for ASCI Shiny app

# brainstorm here, then will later add to R Markdown/Shiny file "Index.Rmd" in ASCIfigs repository 
# 2021, Annie Holt

library(tidyverse)



#### DATA IMPORT ####



# channelized vs natural... unified channel engineering, channeltype(natural vs engineered)
# hard vs natural bottom 
# perennial vs nonperennial 


# getting channel engineering data from the SMC

library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
library(lubridate)

# con is short for connection
# Create connection to the database
# con <- 
#don't post connection info to GitHub, create connection locally 


#for getting master ID
lustations_query <- "select * from sde.lu_stations"
tbl_lustations   <- tbl(con, sql(lustations_query))
lustations.1    <- as.data.frame(tbl_lustations)
lustations.2 <- select(lustations.1,masterid,stationid)

chaneng_query <- "select * from sde.unified_channelengineering"
tbl_chaneng <- tbl(con, sql(chaneng_query))
chaneng.1 <- as.data.frame(tbl_chaneng)

# using masterid
chaneng.2 <- chaneng.1 %>% 
  left_join(lustations.2, by = c("stationcode" = "stationid"))



# # flow status dataset 
# flow <- read_csv("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/Intermittent_ASCI.csv")


#### DATA PREPPING ####


# adding channel type information
# engineered vs natural categories
# engineered subcategories: fully hard, fully earthen, soft bottom with hard sides
# for Shiny, potentially want to be able to toggle between categories 


# STILL NEED NEW FLOW INFORMATION FROM SUSIE (perennial, intermittent, etc)

# channel type information from SMC (unified channel engineering)
# retain fields, conform categories to Natural vs. Engineered (with sub types Hard, Soft, NA for sides and bottom)
# create summarizing fields for Fully Hardened, Fully Earthen, and Soft bottom with one or more sides hard
ascidat_add <- ascidat %>% 
  mutate(date2 = as.character(date)) %>% 
  mutate(date = (as.Date(date2, format = "%m/%d/%y"))) %>% 
  select(-date2) %>% 
  left_join(lustations.2, by = c("site" = "stationid")) %>% 
  left_join(chaneng.2 %>% select(stationcode, masterid,sampledate,channeltype,rightsideofstructure,leftsideofstructure,bottom),
            by = c("masterid" = "masterid", "date" = "sampledate")) %>% 
  mutate(rightside = case_when(rightsideofstructure %in% c("Rock", "Concrete", "Grouted rock", "Grouted Rock", "ROck") ~ "Hard",
                               rightsideofstructure %in% c("Earthen bare", "Vegetative/Natural","Earthen") ~ "Soft",
                               rightsideofstructure %in% c("Other","NR", "NA") ~ "NA", #confirm what to do with "Other" 
                               is.na(rightsideofstructure) ~ "NA")) %>% 
  mutate(leftside = case_when(leftsideofstructure %in% c("Rock", "Concrete", "Grouted rock","Grouted Rock") ~ "Hard",
                              leftsideofstructure %in% c("Earthen", "Earthen bare", "Vegetative/Natural") ~ "Soft",
                              leftsideofstructure %in% c("Other","NR", "NA") ~ "NA", #confirm what to do with "Other" 
                              is.na(leftsideofstructure) ~ "NA")) %>% 
  mutate(bottom = case_when(bottom %in% c("Concrete", "Rock", "Grouted rock") ~ "Hard",
                             bottom %in% c("Soft/Natural") ~ "Soft",
                             bottom %in% c("NA", "NR", "Other") ~ "Hard",
                             is.na(bottom) ~ "NA")) %>% 
  select(-rightsideofstructure, - leftsideofstructure) %>% 
  mutate(rightside = ifelse(rightside == "NA", NA, rightside)) %>% 
  mutate(leftside = ifelse(leftside == "NA", NA, leftside)) %>% 
  mutate(bottom = ifelse(bottom == "NA", NA, bottom)) %>% 
  mutate(channeltype = ifelse(channeltype == "NA", NA, channeltype)) %>% 
  mutate(channeltype_sub = case_when(channeltype == "Engineered" &
                                       rightside == "Hard" & leftside == "Hard" & 
                                       bottom == "Hard" ~ "Fully Hardened",
                                     channeltype == "Engineered" &
                                       rightside == "Soft" & leftside == "Soft" & 
                                       bottom == "Soft" ~ "Fully Earthen",
                                     channeltype == "Engineered" &
                                       rightside == "Soft" & leftside == "Hard" & 
                                       bottom == "Soft" ~ "Soft Bottom, One or Both Sides Hardened",
                                     channeltype == "Engineered" &
                                       rightside == "Hard" & leftside == "Soft" & 
                                       bottom == "Soft" ~ "Soft Bottom, One or Both Sides Hardened",
                                     channeltype == "Engineered" &
                                       rightside == "Hard" & leftside == "Hard" & 
                                       bottom == "Soft" ~ "Soft Bottom, One or Both Sides Hardened")) %>% 
  select(-masterid, -stationcode) %>% 
  mutate(chann = as.factor(channeltype))
  
# rename to ascidat so don't have to update the shiny

# ascidat <- ascidat_add
# save(ascidat, file = "data/ascidat.RData")





# old version:
# asci dataset, with channel condition and flow status data added
# ascidat_add <- ascidat %>% 
#   left_join(gis_metrics.1 %>% select(stationcode,flowstatus),
#             by = c("site" = "stationcode")) %>% 
#   left_join(chaneng.1 %>% select(stationcode, channeltype),
#             by = c("site" = "stationcode")) %>% 
#   mutate(channeltype = as.factor(channeltype)) %>% 
#   distinct(sampleid, ind, .keep_all = TRUE)
# 




ascidat_supp <- ascidat %>%
  left_join(suppdat %>% select(-ind, -scr, -sit1), by = c("sampleid" = "sampleid"))%>%
  distinct(sampleid,ind, .keep_all = TRUE)


#### CHANNELIZED VS NATURAL VISUALIZATIONS ####




# boxplot, x is ref\int\stressed, y is index score, color by channel vs natural

# ascidat_add$channeltype <- factor(ascidat_add$channeltype, levels = c("Natural", "Engineered"))
# 
# ggplot(data = ascidat_add %>%
#          filter(!is.na(channeltype)) %>%
#          filter(!is.na(sit2)),
#        mapping = aes(x = sit2, y = scr))+
#   geom_boxplot()+
#   geom_jitter(aes(color = channeltype), alpha = 0.7)+
#   facet_grid(~ind)+
#   scale_y_continuous('Index Score') +
#   scale_x_discrete(limits = c("Reference", "Intermediate", "Stressed"))+
#   scale_color_brewer(palette = "Accent")+
#   labs(color = "Channel Type")+
#   theme_minimal(base_family = 'serif', base_size = 16) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     panel.border = element_rect(colour = 'black', fill = NA, size = 0.5)
#     # ,
#     # legend.position = 'top'
#   )
# 
# 
# ggsave("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/asci_channeltype_stressor.jpeg",
#        dpi = 150,height = 14, width = 25, units = "cm")


# log for total nitrogen, total phosphorus? other x scales/limits?

ascidat_supp$channeltype <- factor(ascidat_supp$channeltype, levels = c("Natural", "Engineered"))

# "LogWSA"
# "Nitrogen_Total_mgPerL"
# "PCT_SAFN"
# "Phosphorus_as_P_mgPerL"
# "SpecificConductivity_uSPercm"
# "Temperature_Deg_C"
# "XCDENMID" 
# "XSLOPE"

# c('Phosphorus (mg/L)', 'Total Nitrogen (mg/L)', '% Shading', '% Sands and Fines', 'Conductivity (uS/cm)',
#   'Temperature (C)', 'Mean Slope of Reach (%)', 'log-Watershed Area (m2)', 'Elevation (m)')


ggplot(data = ascidat_supp %>% 
         filter(!is.na(channeltype)) %>% 
         filter(!is.na(sit2)) %>% 
         left_join(ascithr %>% mutate(tax = recode(tax, "Soft-bodied" = "SBA")) %>% select(tax,bigls,bival)) %>% 
         filter(bigls == "Ref10"), 
       mapping = aes(x = Nitrogen_Total_mgPerL, y = scr, color = channeltype)) +
  geom_point(alpha = 0.8, size = 2)+
  geom_smooth(method = 'lm', se = FALSE)+
  geom_hline(aes(yintercept = bival), linetype = 'dashed', colour = 'orange', size = 1)+
  facet_grid(~ind)+
  scale_x_log10()+
  scale_y_continuous('ASCI score')+
  scale_color_brewer(palette = "Accent")+
  xlab("Nitrogen (mg/L)")+
  labs(color = "Channel Type")+
  theme_minimal(base_family = 'serif', base_size = 16) + 
  theme(panel.border = element_rect(colour = 'black', fill = NA, size = 0.5),panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


# ggsave("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/asci_channeltype_nitrogen.tiff",
#        dpi = 150,height = 14, width = 25, units = "cm")
# 
# ggsave("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/asci_channeltype_nitrogen.jpeg",
#        dpi = 150,height = 14, width = 25, units = "cm")


#### PERENNAIL VS NONPERENNIAL VISUALIZATIONS ####


# ascidat_add$flowstatus <- factor(ascidat_add$flowstatus, levels = c("Perennial", "Nonperennial"))
# 
# ggplot(data = ascidat_add %>% 
#          filter(flowstatus == "Perennial" | flowstatus == "Nonperennial") %>% 
#          left_join(ascithr %>% mutate(tax = recode(tax, "Soft-bodied" = "SBA")) %>% select(tax,bigls,bival)) %>% 
#          filter(bigls == "Ref10"),
#        mapping = aes(x = sit2, y = scr))+
#   geom_boxplot()+
#   geom_jitter(aes(color = flowstatus), size = 2, alpha = 0.3)+
#   geom_hline(aes(yintercept = bival), linetype = 'dashed', colour = 'royalblue', size = 1)+
#   facet_grid(~ind)+
#   scale_y_continuous('Index Score') +
#   scale_x_discrete(limits = c("Reference", "Intermediate", "Stressed"))+ 
#   scale_color_brewer(palette = "Dark2")+
#   labs(color = "Flow Status")+
#   theme_minimal(base_family = 'serif', base_size = 16) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1), 
#         axis.title.x = element_blank(), 
#         panel.border = element_rect(colour = 'black', fill = NA, size = 0.5)
#         # ,
#         # legend.position = 'top'
#   )
# 
# 
# # ggsave("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/asci_flowstatus.tiff",
# #        dpi = 150,height = 18, width = 25, units = "cm")
# # ggsave("C:/Users/anneh/OneDrive - SCCWRP/AH_task_files/Susie/asci_flowstatus.jpeg",
# #        height = 18, width = 25, units = "cm")
# 
# 
# # "LogWSA"
# # "Nitrogen_Total_mgPerL"
# # "PCT_SAFN"
# # "Phosphorus_as_P_mgPerL"
# # "SpecificConductivity_uSPercm"
# # "Temperature_Deg_C"
# # "XCDENMID" 
# # "XSLOPE"
# 
# # c('Phosphorus (mg/L)', 'Total Nitrogen (mg/L)', '% Shading', '% Sands and Fines', 'Conductivity (uS/cm)',
# #   'Temperature (C)', 'Mean Slope of Reach (%)', 'log-Watershed Area (m2)', 'Elevation (m)')
# 
# ascidat_supp$flowstatus <- factor(ascidat_supp$flowstatus, levels = c("Perennial", "Nonperennial"))
# 
# ggplot(data = ascidat_supp %>% 
#          filter(flowstatus == "Perennial" | flowstatus == "Nonperennial"), 
#        mapping = aes(x = Nitrogen_Total_mgPerL, y = scr, color = flowstatus)) +
#   geom_point(alpha = 0.3)+
#   geom_smooth(method = 'lm', se = FALSE)+
#   facet_grid(~ind)+
#   scale_x_log10()+
#   scale_y_continuous('ASCI score')+
#   scale_color_brewer(palette = "Dark2")+
#   xlab("Nitrogen (mg/L)")+
#   labs(color = "Flow Status")+
#   theme_minimal(base_family = 'serif', base_size = 16) + 
#   theme(panel.border = element_rect(colour = 'black', fill = NA, size = 0.5),
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank())



