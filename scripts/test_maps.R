library(tidyverse)
library(sf)
library(httr)
library(ows4R)
library(ggsn)
library(classInt)

#### test requesting wfs from geosever ####
wfs_shmp <- "https://geonode.tdis.io/geoserver/SHMP/wfs?"
url <- parse_url(wfs_shmp)

#request disaster declarations
url$query <- list(service = "wfs",
                  request = "GetFeature",
                  version = "2.0.0",
                  typename = "Total_Disaster_Declarations",
                  srsName = "EPSG:3083"
)
request <- build_url(url)
dd <- st_read(request) 
st_crs(dd) = 3083
dd <- dd %>% 
  filter(fid != 0)

#tdem regions
wfs_geonode <- "https://geonode.tdis.io/geoserver/geonode/wfs?"
url <- parse_url(wfs_geonode)
url$query <- list(service = "wfs",
                  request = "GetFeature",
                  version = "2.0.0",
                  typename = "tdem_regions",
                  srsName = "EPSG:3083"
                  )
                  
request <- build_url(url)
tdem <- st_read(request) 
st_crs(tdem) = 3083
tdem <- st_cast(tdem, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")

#### set cuts and palette ####
#try different breaks
brks.qt = classIntervals(dd$total_disasters, n = 5, style = "quantile", dataPrecision = 1)
# brks.jk = classIntervals(dd$total_disasters, n = 5, style = "jenks", dataPrecision = 1)
# brks.eq = classIntervals(dd$total_disasters, n = 5, style = "equal", dataPrecision = 1)

# create cuts based on quantile breaks
dd <- dd %>% 
  mutate(total_disa_cut = cut(total_disasters, breaks = brks.qt$brks, include.lowest = TRUE))

pal <- c("#C8D1D3", "#A8C6C9", "#87BCBF", "#5C828A", "#324755")

#### state level ####
map <- ggplot() + 
  geom_sf(dd, mapping=aes(fill = total_disa_cut), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill",name="Total Disaster Declarations",labels = c(" ≤ 11", "13", "15", "18","28")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "(from 1953-2021)") +
  theme(
        plot.background = element_rect(fill = "#F0F3F4", color = NA),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.title = element_text(size=11, vjust = 1),
        legend.spacing.y = unit(1, "mm"),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.2, vjust = 7.5)
  )

map 

ggsave("/Users/lstearns/projects/shmp/figures/disasters_state.png", map, height = 5, width = 7, dpi = 300)

plotly::ggplotly(map) # it's pretty rough going straight from ggplot to plotly

#try one with less theme formatting
map_basic <- ggplot() + 
  geom_sf(dd, mapping=aes(fill = total_disa_cut), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  theme_void() + 
  scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill",name="Total Disaster \n Declarations",labels = c(" ≤ 11", "13", "15", "18","28"))

plotly::ggplotly(map_basic) #better but still a little wonky, legend formatting will probably need to happen in plotly 


# #### region 2 level map ####
# #join regions to disaster declarations 
# tdem_2 <- tdem %>% 
#   filter(region_num == 2)
# 
# dd_2 <- dd %>% 
#   mutate(centroid = st_centroid(st_geometry(dd))) %>% 
#   st_join(tdem, left = TRUE, largest = TRUE) %>%
#   filter(region_num == 2)
# 
# map2 <- ggplot() + 
#   geom_sf(dd_2, mapping=aes(fill = total_disa_cut), color = NA) +
#   geom_sf(data=tdem_2, fill = NA)+
#   north(dd_2, symbol = 10, location = "bottomright")+
#   theme_void() + 
#   scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill",name="Total Disaster Declarations",labels = c(" ≤ 11", "13", "15", "18","28"))+
#   guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
#   # annotate("text", x = 0,  y = 0, label = "(from 1953-2021)", size = 3)+
#   labs(caption = "Region 2 (from 1953-2021)")+
#   theme(legend.position = "bottom",
#         plot.background = element_rect(fill = "#F0F3F4", color = NA),
#         plot.margin = unit(c(1,2,1,2), "cm"),
#         panel.background = element_rect(fill = "#F0F3F4", color = NA),
#         legend.background = element_rect(fill = "#F0F3F4", color = NA),
#         legend.title = element_text(size=11, vjust = 1),
#         legend.spacing.y = unit(1, "mm"),
#         plot.caption.position = "panel",
#         plot.caption = element_text(hjust = -0.4 , vjust = 7.5)
#   )
# 
# map2 
# 
# ggsave("/Users/lstearns/projects/shmp/figures/disasters_reg2.png", map, height = 5, width = 7, dpi = 300)
# 
# plotly::ggplotly(map2)



#### Style guide examples ####

#get sheldus impacts from geoserver
wfs_shmp <- "https://geonode.tdis.io/geoserver/SHMP/wfs?"
url <- parse_url(wfs_shmp)

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  version = "2.0.0",
                  typename = "sheldus_impacts_counties",
                  srsName = "EPSG:3083"
)
request <- build_url(url)
sheldus <- st_read(request) 
st_crs(sheldus) = 3083


#### Lightning ####
#try different breaks
lightning_qt = classIntervals(sheldus$property_dmg_lightning, n = 5, style = "quantile", dataPrecision = 1)

lightning_sd = classIntervals(sheldus$property_dmg_percap_lightning, n = 5, style = "sd")

lightning_pretty = classIntervals(sheldus$property_dmg_percap_lightning, n = 5, style = "pretty")

lightning_days_pretty = classIntervals(sheldus$duration_days_lightning, n = 5, style = "pretty")

lightning_days_jenk = classIntervals(sheldus$duration_days_lightning, n = 5, style = "jenks")


# create cuts based on quantile breaks

sheldus_map <- sheldus %>% 
  mutate(lightning_jnk = cut(duration_days_lightning, breaks = lightning_days_jenk$brks, include.lowest = TRUE))

lightning_5 <- c("#44D1DD", "#58E0C5", "#78EDA9", "#A2F885", "#DFFC4E")


map_lightning <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = lightning_jnk), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(lightning_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Lightning Strike",labels = c(" ≤ 1", "4", "9", "20", "39")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Event Days") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0.1, vjust = 7.5)
  )

map_lightning

ggsave("/Users/lstearns/projects/shmp/figures/lightning_days.png", map_lightning, height = 5, width = 7, dpi = 300)

#### Winter Weather ####
winter_dmg_qt=classIntervals(sheldus$property_dmg_percap_winter_weather, n=5, style = "quantile", dataPrecision = 1)

winter_dmg_pretty=classIntervals(sheldus$property_dmg_percap_winter_weather, n=5, style = "pretty", dataPrecision = 1)


sheldus_map <- sheldus %>% 
  mutate(winter_pretty = cut(duration_days_winter_weather, breaks = winter_pretty$brks, include.lowest = TRUE),
         winter_dmg_brk = cut(property_dmg_percap_winter_weather, breaks = c(0, 50, 100, 1000, 5000, 20000), include.lowest = TRUE))

winter_5 <- c("#CAF0F8", "#00B4D8", "#A782F8", "#F88BF8", "#B11B64")

map_winter <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = winter_dmg_brk), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(winter_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Severe Winter Weather",labels = c(" ≤ $50", "$100", "$1,000", "$5,000", "$20,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.1, vjust = 7.5)
  )

map_winter
ggsave("/Users/lstearns/projects/shmp/figures/winter_dmg.png", map_winter, height = 5, width = 7, dpi = 300)

#### Wild Fire ####
fire_dmg_qt=classIntervals(sheldus$property_dmg_percap_fire, n=5, style = "quantile", dataPrecision = 1)

fire_dmg_pretty=classIntervals(sheldus$property_dmg_percap_fire, style = "pretty", dataPrecision = 1)


fire_5 <- c("#FEEE90", "#FA9336", "#EC3F26", "#B8181C", "#581029")

sheldus_map <- sheldus %>% 
  mutate(fire_dmg_brk = cut(property_dmg_percap_fire, breaks = c(0, 50, 200, 1000, 5000, 13000), include.lowest = TRUE),
         fire_dmg_brk_2 = cut(property_dmg_percap_fire, breaks = c(0, 2, 10, 33, 155, 12320), include.lowest = TRUE))

map_fire <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = fire_dmg_brk_2), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(fire_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Wild Fire         ",labels = c(" ≤ $2", "$10", "$33", "$155", "$13,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.2, vjust = 7.5)
  )

map_fire

ggsave("/Users/lstearns/projects/shmp/figures/fire_dmg_qt.png", map_fire, height = 5, width = 7, dpi = 300)

sheldus_map <- sheldus %>% 
  mutate(fire_dmg_brk_zero = cut(property_dmg_percap_fire, breaks = c(0, 1, 50, 200, 1000, 5000, 13000), include.lowest = TRUE))

fire_6 <- c("#A7A7A7", "#FEEE90", "#FA9336", "#EC3F26", "#B8181C", "#581029")

map_fire_zeros<- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = fire_dmg_brk_zero), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(fire_6, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Wild Fire         ",labels = c("0", "$50", "$200", "$1,000", "$5,000", "$13,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.2, vjust = 5)
  )

map_fire_zeros

ggsave("/Users/lstearns/projects/shmp/figures/fire_dmg_zeros.png", map_fire_zeros, height = 5, width = 7, dpi = 300)

#### Drought ####
drought_crop_dmg_qt=classIntervals(sheldus$crop_dmg_percap_drought,n=5, style = "quantile", dataPrecision = 1)
drought_crop_dmg_jnk=classIntervals(sheldus$crop_dmg_percap_drought, n=5, style = "jenks", dataPrecision = 1)
drought_crop_dmg_prty=classIntervals(sheldus$crop_dmg_percap_drought, style = "pretty", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  mutate(drought_crop_brk = cut(crop_dmg_percap_drought, breaks = c(0, 50, 500, 5000, 50000, 200000), include.lowest = TRUE))

drought_5 <- c("#88A043", "#F0BE63", "#E49544", "#BE532E", "#9B3B28")

map_drought <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = drought_crop_brk), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(drought_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Drought         ",labels = c(" ≤ $50", "$500", "$5,000", "$50,000", "$200,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Crop Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.2, vjust = 7.5)
  )

map_drought
ggsave("/Users/lstearns/projects/shmp/figures/drought_crop_dmg.png", map_drought, height = 5, width = 7, dpi = 300)

#### Tornado ####
tornado_dmg_qt=classIntervals(sheldus$property_dmg_percap_tornado,n = 6,style = "quantile", dataPrecision = 1)
tornado_dmg_jnk=classIntervals(sheldus$property_dmg_percap_tornado, style = "jenks", dataPrecision = 1)
tornado_dmg_prty=classIntervals(sheldus$property_dmg_percap_tornado, style = "pretty", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  mutate(tornado_dmg_brk = cut(property_dmg_percap_tornado, breaks = c(0, 0.3, 5.5, 22.5, 49.5, 133, 3052), include.lowest = TRUE))

tornado_5 <- c("#94A6BD","#628A7E", "#FAF1C3", "#E77A62", "#913427")
tornado_6 <- c("#94A6BD", "#484467", "#628A7E", "#FAF1C3", "#E77A62", "#913427")

map_tornado <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = tornado_dmg_brk), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(tornado_6, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Tornado         ",labels = c(" ≤ $0.50", "$5", "$20", "$50", "$130","$3050")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.3, vjust = 7.5)
  )

map_tornado
ggsave("/Users/lstearns/projects/shmp/figures/tornado_dmg_qt.png", map_tornado, height = 5, width = 7, dpi = 300)

#### Wind ####
wind_day_prty=classIntervals(sheldus$duration_days_wind, n = 5, style = "pretty", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  mutate(wind_day_brk = cut(duration_days_wind, breaks = wind_day_prty$brks, include.lowest = TRUE))

wind_5 <- c("#BEE0FB","#8CC1D6", "#4E8EAB", "#356F93", "#0E2F49")

map_wind <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = wind_day_brk), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(wind_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Wind         ",labels = c(" ≤ 20", "40", "60", "80", "100")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Event Days") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, vjust = 7.5)
  )

map_wind
ggsave("/Users/lstearns/projects/shmp/figures/wind_days.png", map_wind, height = 5, width = 7, dpi = 300)


#### Flood ####
flood_dmg_qt=classIntervals(sheldus$property_dmg_percap_flood,n=5, style = "quantile", dataPrecision = 1)
flood_dmg_jk=classIntervals(sheldus$property_dmg_percap_flood,n=5, style = "jenks", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  mutate(flood_dmg_qt = cut(property_dmg_percap_flood, breaks = c("0","50", "100", "500", "10000", "50000"), include.lowest = TRUE))

flood_5 <- c("#ADEBFD","#8ECEFB", "#71B1F0", "#5497D3", "#237BBA")

map_flood <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = flood_dmg_qt), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(flood_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Flood           ",labels = c(" ≤ $50", "$100", "$500", "$10,000", "$50,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.3, vjust = 7.5)
  )

map_flood
ggsave("/Users/lstearns/projects/shmp/figures/flood_dmg.png", map_flood, height = 5, width = 7, dpi = 300)


####Hurricanes####
hurricane_dmg_qt=classIntervals(sheldus$property_dmg_percap_hurricane, style = "quantile", dataPrecision = 1)
hurricane_dmg_jk=classIntervals(sheldus$property_dmg_percap_hurricane,n=6, style = "jenks", dataPrecision = 1)
hurricane_dmg_pt=classIntervals(sheldus$property_dmg_percap_hurricane,n=6, style = "pretty")

sheldus_map <- sheldus %>% 
  mutate(hurricane_dmg = cut(property_dmg_percap_hurricane, breaks = c("0","10", "50", "500", "5000", "10000", "72735"), include.lowest = TRUE))

hurricane_6 <- c("#74E8D4","#49BBF8", "#FCC040", "#F25B5E", "#CB29AD", "#32249B")

map_hurricane <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = hurricane_dmg), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(hurricane_6, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Hurricane         ",labels = c(" ≤ $10", "$50", "$500", "$5,000", "$10,000", "$70,000")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Per Capita Property Damage") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = -0.3, vjust = 7.5)
  )

map_hurricane
ggsave("/Users/lstearns/projects/shmp/figures/hurricane_dmg.png", map_hurricane, height = 5, width = 7, dpi = 300)

#### hailstorm ####
hail_days_qt=classIntervals(sheldus$duration_days_hail,n=5, style = "quantile", dataPrecision = 1)


sheldus_map <- sheldus %>% 
  mutate(hail_days= cut(duration_days_hail, breaks = hail_days_qt$brks, include.lowest = TRUE))

hail_5<- c("#BEE0FB","#7BC8E4", "#F97DA0", "#A782F8", "#4D4AA3")

map_hail <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = hail_days), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(hail_5, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Hail           ",labels = c(" ≤ 1", "3", "6", "14", "50")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Event Days") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, vjust = 7.5)
  )

map_hail
ggsave("/Users/lstearns/projects/shmp/figures/hail_days.png", map_hail, height = 5, width = 7, dpi = 300)

#### severe storm ####
storm_days_qt=classIntervals(sheldus$duration_days_severe_storm,n=6, style = "quantile", dataPrecision = 1)


sheldus_map <- sheldus %>% 
  mutate(storm_days= cut(duration_days_severe_storm, breaks = c("0","5", "10", "15", "30", "50", "95"), include.lowest = TRUE))

storm_6<- c("#BEE0FB","#3D92CB", "#6D9656", "#F8B229","#EA4630", "#BE1F1D")

map_storm <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = storm_days), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(storm_6, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name="Severe Storms",labels = c(" ≤ 5", "10", "15", "30", "50", "95")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "Event Days") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=11, vjust = 1, hjust = -0.1),
    legend.spacing.y = unit(1, "mm"),
    legend.spacing.x = unit(2, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0, vjust = 7.5)
  )

map_storm
ggsave("/Users/lstearns/projects/shmp/figures/storm_days.png", map_storm, height = 5, width = 7, dpi = 300)

#### grey scale disaster declarations ####
grey_5 <- c("#F1E8E4","#B6B1B7", "#9A97A2", "#676678", "#27283C")

map_grey <- ggplot() + 
  geom_sf(dd, mapping=aes(fill = total_disa_cut), color = NA) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(grey_5, "#1B1C20"), aesthetics = "fill",name="Total Disaster Declarations",labels = c(" ≤ 11", "13", "15", "18","28")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(caption = "(from 1953-2021)") +
  theme(
    plot.background = element_rect(fill = "#F0F3F4", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#F0F3F4", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#F0F3F4", color = NA),
    legend.title = element_text(size=11, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(hjust = 0.2, vjust = 7.5)
  )

map_grey 

ggsave("/Users/lstearns/projects/shmp/figures/grey_declarations.png", map_grey, height = 5, width = 7, dpi = 300)






#### Updated Color Palette and Template ####
#### Lightning ####
#try different breaks
lightning_qt = classIntervals(sheldus$property_dmg_lightning, n = 5, style = "quantile", dataPrecision = 1)

# create cuts based on quantile breaks

sheldus_map <- sheldus %>% 
  mutate(lightning_brks = cut(property_dmg_lightning, breaks = c("0","1","5000","30000","100000","500000","12046977"), include.lowest = TRUE))

lightning_5_zero <- c("#FFFFFF", "#FFF9AF", "#E8D744", "#78C59B", "#25838D", "#0C3F60")

map_lightning <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = lightning_brks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(lightning_5_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0","5K", "30K", "100K", "500K", "12M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Lightning Strikes",
            subtitle = "From 2000-2020",
            caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

map_lightning

# ggsave("/Users/lstearns/projects/shmp/figures/update/lightning_days.pdf", map_lightning, height = 5, width = 7)
ggsave("/Users/lstearns/projects/shmp/figures/update/lightning_days.png", map_lightning, height = 5, width = 7)

tx <- tigris::states() %>% tigris::filter_state("Texas")
tx <- st_transform(tx, 3083)

#without tdem regions
map_lightning_tx <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = lightning_brks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tx, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(lightning_5_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0","5K", "30K", "100K", "500K", "12M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Lightning Strikes",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )
ggsave("/Users/lstearns/projects/shmp/figures/update/lightning_days_tx.png", map_lightning_tx, height = 5, width = 7)

#### Winter Weather ####
sheldus_map <- sheldus %>% 
  mutate(winter_breaks = cut(property_dmg_winter_weather, breaks = c("0","1","10000","50000","200000","1000000","235267185"), include.lowest = TRUE))

winter_zero <- c("#FFFFFF", "#BEE0FB", "#839DBE", "#936193", "#DDA0B0", "#A72649")

map_winter <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = winter_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(winter_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0","10K", "50K", "200K", "1M", "200M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Winter Weather",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )
ggsave("/Users/lstearns/projects/shmp/figures/update/winter.png", map_winter, height = 5, width = 7)

#### Fire ####
classIntervals(sheldus$property_dmg_fire, style = "quantile", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  mutate(fire_breaks = cut(property_dmg_fire, breaks = c("0","1","50000","200000","1000000","2000000","582646052"), include.lowest = TRUE))

fire_zero <- c("#FFFFFF", "#FEEE90", "#FA9336", "#EC3F26", "#B8181C", "#581029")

map_fire <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = fire_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(fire_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0","50K", "200K", "1M", "2M", "600M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Wild Fire",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/fire.png", map_fire, height = 5, width = 7)

#### Drought ####
classIntervals(sheldus$crop_dmg_drought, style = "quantile", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  select(crop_dmg_drought, the_geom) %>% 
  mutate(drought_breaks = cut(crop_dmg_drought, breaks = c("0","1","100000","1000000","10000000","100000000","2570570820"), include.lowest = TRUE))

drought_zero <- c("#FFFFFF", "#25644E", "#88A043", "#A2BE85", "#F0BE63", "#9B3B28")

map_drought <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = drought_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(drought_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "100K", "1M", "10M", "100M", "2.5B")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Crop Damage due to Drought",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/drought.png", map_drought, height = 5, width = 7)

#### Hurricane ####
classIntervals(sheldus$property_dmg_hurricane, style = "quantile", dataPrecision = 1)

sheldus_map <- sheldus %>% 
  select(property_dmg_hurricane, the_geom) %>% 
  mutate(hurricane_breaks = cut(property_dmg_hurricane, breaks = c("0","1","50000","500000","1000000","100000000","20509044751"), include.lowest = TRUE))

hurricane_zero <- c("#FFFFFF", "#96ffea", "#7ae5e1", "#64c9ea", "#f0be63", "#f25b5e")

map_hurricane <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = hurricane_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(hurricane_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "50K", "500K", "1M", "100M", "20B")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Hurricanes",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/hurricane.png", map_hurricane, height = 5, width = 7)

#### Tornado ####
classIntervals(sheldus$property_dmg_tornado, style = "quantile", dataPrecision = 1, n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_tornado, the_geom) %>% 
  mutate(tornado_breaks = cut(property_dmg_tornado, breaks = c("0","1","15000","200000","1000000","5000000","2473887152"), include.lowest = TRUE))

tornado_zero <- c("#FFFFFF", "#94A6BD", "#628A7E","#FAF1C3","#E77A62", "#913427")

map_tornado <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = tornado_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(tornado_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "15K", "200K", "1M", "500M", "2.5B")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Tornadoes",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/tornado.png", map_tornado, height = 5, width = 7)

#### Wind ####

classIntervals(sheldus$property_dmg_wind, style = "quantile", dataPrecision = 1, n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_wind, the_geom) %>% 
  mutate(wind_breaks = cut(property_dmg_wind, breaks = c("0","1","100000","500000","1000000","10000000","272044887"), include.lowest = TRUE))

wind_zero <- c("#FFFFFF", "#FEF0FE", "#D4B8D4","#9D839E","#6A516A", "#3A243B")

map_wind <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = wind_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(wind_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "100K", "500K", "1M", "10M", "300M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Wind",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/wind.png", map_wind, height = 5, width = 7)

#### Flooding ####

classIntervals(sheldus$property_dmg_flood, style = "quantile", dataPrecision = 1, n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_flood, the_geom) %>% 
  mutate(flood_breaks = cut(property_dmg_flood, breaks = c("0","1","50000","500000","1500000","10000000","15385714889"), include.lowest = TRUE))

flood_zero <- c("#FFFFFF", "#BEE0FB", "#8CC1D6","#4E8EAB","#356F93", "#0E2F49")

map_flood <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = flood_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(flood_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "50K", "500K", "1.5M", "10M", "15B")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Flooding",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/flood.png", map_flood, height = 5, width = 7)

##### Hail ####

classIntervals(sheldus$property_dmg_hail, style = "quantile", dataPrecision = 1,n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_hail, the_geom) %>% 
  mutate(hail_breaks = cut(property_dmg_hail, breaks = c("0","1","15000","150000","500000","2000000","1644217358"), include.lowest = TRUE))

hail_zero <- c("#FFFFFF", "#BEE0FB", "#839DBE","#BC86BE","#714871", "#A72649")

map_hail <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = hail_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(hail_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "15K", "150K", "500K", "2M", "1.6B")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Hail",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/hail.png", map_hail, height = 5, width = 7)



##### Severe ####

classIntervals(sheldus$property_dmg_severe_storm, style = "quantile", dataPrecision = 1, n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_severe_storm, the_geom) %>% 
  mutate(severe_breaks = cut(property_dmg_severe_storm, breaks = c("0","1","150000","300000","500000","1000000","236114887"), include.lowest = TRUE))

severe_zero <- c("#FFFFFF", "#C6EBDF", "#82A69A","#4E828D","#D9AD59", "#9D2716")

map_severe <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = severe_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(severe_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "150K", "300K", "500K", "1M", "200M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Severe Storms",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/severe.png", map_severe, height = 5, width = 7)


##### Earthquake ####

classIntervals(sheldus$dura, style = "quantile", dataPrecision = 1, n=5)

sheldus_map <- sheldus %>% 
  select(property_dmg_severe_storm, the_geom) %>% 
  mutate(severe_breaks = cut(property_dmg_severe_storm, breaks = c("0","1","150000","300000","500000","1000000","236114887"), include.lowest = TRUE))

earthquake_zero <- c('#FFFFFF', '#c6ebdf', '#a3b9af', '#9a8279', '#7a5446', '#5a2614')

map_severe <- ggplot() + 
  geom_sf(sheldus_map, mapping=aes(fill = severe_breaks), color = "#D6D6D6", size=0.2) +
  geom_sf(data=tdem, fill = NA) +
  north(dd, symbol = 10, location = "bottomright") +
  theme_void() + 
  scale_color_manual(values =  c(severe_zero, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "150K", "300K", "500K", "1M", "200M")) +
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  labs(title = "Property Damage due to Severe Storms",
       subtitle = "From 2000-2020",
       caption = "Source: Spatial Hazards Events and Losses Database (SHELDUS)") +
  theme(
    plot.background = element_rect(fill = "#EFEFEF", color = NA),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.position = "bottom",
    legend.background = element_rect(fill = "#EFEFEF", color = NA),
    legend.title = element_text(size=10, vjust = 1),
    legend.spacing.y = unit(1, "mm"),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 7, face = "italic"),
    plot.subtitle = element_text(size = 9, face = "italic")
  )

ggsave("/Users/lstearns/projects/shmp/figures/update/severe.pdf", map_severe, height = 5, width = 7)