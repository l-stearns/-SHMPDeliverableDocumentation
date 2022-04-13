library(tidyverse)
library(sf)
library(httr)
library(ows4R)
library(ggsn)

#### read wfs from geoserver ####
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


#### region 2 level map ####
#join regions to disaster declarations 
tdem_2 <- tdem %>% 
  filter(region_num == 2)

dd_2 <- dd %>% 
  mutate(centroid = st_centroid(st_geometry(dd))) %>% 
  st_join(tdem, left = TRUE, largest = TRUE) %>%
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

ggsave("/Users/lstearns/projects/shmp/figures/disasters_reg2.png", map, height = 5, width = 7, dpi = 300)

plotly::ggplotly(map2)
