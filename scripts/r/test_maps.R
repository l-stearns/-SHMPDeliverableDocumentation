library(tidyverse)
library(sf)
library(classInt)
library(AzureStor)

blob_endp_key <- storage_endpoint("https://shmpstorage.blob.core.windows.net/", key = "9bZuZnSZpd8Nciqu7noV+iE4z5k7MsUJ2bAx7eL0hXemMNq3RzO1ZqsTasQw/forzCoVnD8++gaaJ5OMCSxXWg==")
cleanfiles <- storage_container(blob_endp_key, "cleanfiles")

storage_multidownload(cleanfiles, src="/boundaries/tdem_regions.*", dest = "/Users/lstearns/Downloads")

tdem <- st_read("/Users/lstearns/Downloads/tdem_regions.shp")

#### read wfs geoserver ####
library(httr)
library(ows4R)

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
dd <- dd %>% 
  filter(fid != 0)

# dd <- st_read("/Users/lstearns/Downloads/Total_Disaster_Declarations/Total_Disaster_DeclarationsPolygon.shp")


#try different breaks
brks.qt = classIntervals(dd$total_disasters, n = 5, style = "quantile", dataPrecision = 1)
brks.jk = classIntervals(dd$total_disasters, n = 5, style = "jenks", dataPrecision = 1)
brks.eq = classIntervals(dd$total_disasters, n = 5, style = "equal", dataPrecision = 1)

# create cuts based on quantile breaks
dd <- dd %>% 
  mutate(total_disa_cut = cut(total_disasters, breaks = brks.qt$brks, include.lowest = TRUE))

pal <- c("#C8D1D3", "#A8C6C9", "#87BCBF", "#5C828A", "#324755")

library(ggsn)

#state level map
map <- ggplot() + 
  geom_sf(dd, mapping=aes(fill = total_disa_cut), color = NA) +
  geom_sf(data=tdem, fill = NA)+
  north(dd, symbol = 10, location = "bottomright")+
  theme_void() + 
  scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill",name="Total Disaster Declarations",labels = c(" ≤ 11", "13", "15", "18","28"))+
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  # annotate("text", x = 0,  y = 0, label = "(from 1953-2021)", size = 3)+
  labs(caption = "(from 1953-2021)")+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F3F4", color = NA),
        plot.margin = unit(c(1,1,1,1), "cm"),
        panel.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.title = element_text(size=11, vjust = 1),
        legend.spacing.y = unit(1, "mm"),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = 0.2, vjust = 7.5)
  )

map 

ggsave("/Users/lstearns/Desktop/disasters_state.png", map, height = 5, width = 7, dpi = 300)

#region 2 level map
tdem_2 <- tdem %>% 
  filter(region_num == 2)

#join regions to disaster declarations 
dd <- dd %>% 
  mutate(centroid = st_centroid(st_geometry(dd))) %>% 
  st_join(tdem, left = TRUE, largest = TRUE)

dd_2 <- dd %>% 
  filter(region_num == 2)

map2 <- ggplot() + 
  geom_sf(dd_2, mapping=aes(fill = total_disa_cut), color = NA) +
  geom_sf(data=tdem_2, fill = NA)+
  north(dd_2, symbol = 10, location = "bottomright")+
  theme_void() + 
  scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill",name="Total Disaster Declarations",labels = c(" ≤ 11", "13", "15", "18","28"))+
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + 
  # annotate("text", x = 0,  y = 0, label = "(from 1953-2021)", size = 3)+
  labs(caption = "Region 2 (from 1953-2021)")+
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "#F0F3F4", color = NA),
        plot.margin = unit(c(1,2,1,2), "cm"),
        panel.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.background = element_rect(fill = "#F0F3F4", color = NA),
        legend.title = element_text(size=11, vjust = 1),
        legend.spacing.y = unit(1, "mm"),
        plot.caption.position = "panel",
        plot.caption = element_text(hjust = -0.4 , vjust = 7.5)
  )

map2 

plotly::ggplotly(map2)


ggsave("/Users/lstearns/Desktop/disasters_reg2.png", map2, height = 5, width = 7, dpi = 300)


# map_qt <- ggplot() + 
#   geom_sf(dd, mapping=aes(fill = total_disa_cut)) +
#   geom_sf(tdem, fill = NA, size = 1)+
#   theme_void() + 
#   scale_color_manual(values =  pal, aesthetics = "fill",name="Total Disaster\nDeclarations",labels = c("11", "13", "15", "18","28"))
# map_qt
# 
# 
# map_qt <- ggplot() + 
#   geom_sf(dd, mapping = aes(fill = total_disa_cut),color = NA) + 
#   geom_sf(data =tdem, fill = NA)+
#   theme_void() +
#   scale_color_manual(values =  c(pal, "#1B1C20"), aesthetics = "fill")
# 
# map_qt+
#   blank()+
#   scalebar(dd, dist = 500, dist_unit = "mi", transform = TRUE, model = "WGS84")+
#   north(dd, symbol = 10)

  
####IA####

base_url <- "https://www.fema.gov/api/open/v1/IndividualsAndHouseholdsProgramValidRegistrations"
filters <- filters <- "?$inlinecount=allpages&$top=1000&$filter=damagedStateAbbreviation eq 'TX'"
  


####Levees####
levee_area <- st_read("/Users/lstearns/Downloads/levees-shp/layers/POLYGON.shp")
mapview::mapview(levee_area)
