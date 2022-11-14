library(AzureStor)
library(sf)
library(tidyverse)
library(RPostgres)
library(ggsn)
library(classInt)

setwd("D:/Projects/SHMP/")

#connect to geonode db for queries
db <- dbConnect(
  Postgres(),
  user = 'my_geonode_data',
  password = getPass::getPass(),
  dbname = 'my_geonode_data',
  host = 'geonode.tdis.io',
  port = 5432
)

#palette for damage figures (categorical)
dmg_pal <- c("#FFFFFF","#C6EBDF","#82A69A", "#2A6580","#D9AD59","#CB7622","#9D2716") 

#palette for ranking figures (continuous)
rank_pal <- c("#9D2716", "#CB7622", "#D9AD59", "#F8DC8F", "#2A6580", "#4E828D","#82A69A","#A4C9BD","#C6EBDF")

#tdem region boundaries 
tdem <- st_read(db, "tdem_regions") 

#### total hazard rank query for wind events ####
rank <- st_read(db, query="
  select
	cz_name,
	rank () over (order by
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.damage_total_adj22 end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_direct end),0)+ 
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_indirect end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_direct end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_indirect end),0)+
		count(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.event_id end) desc ) total_rank,
	rank () over (order by
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_direct end),0)+ 
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_indirect end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_direct end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_indirect end),0)+
		count(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.event_id end) desc ) injury_fatality_event_rank,	
	rank () over (order by
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_direct end),0)+
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.deaths_indirect end),0) desc ) fatality_rank,	
	rank () over (order by
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_direct end),0)+ 
		coalesce(sum(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.injuries_indirect end),0) desc ) injury_rank,
	rank () over (order by
		count(case when event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind') then ne.event_id end) desc ) event_rank,
		counties.the_geom as geom
  from ncei_events as ne
  right join counties 
  	on
  	ne.cz_name  = upper(counties.name)
  group by cz_name, geom")

rank <- rank %>% 
  mutate_if(is_numeric,as.numeric) #numeric fields are read in as integer, convert them to numeric

#choropleth map with continuous scale (ranked from high to low)
map_rank <- ggplot() + #map object
  geom_sf(rank, mapping=aes(fill = total_rank), color = "#D6D6D6", size=0.2) + #counties: fill color by total rank, set outline color, and set line weight
  geom_sf(data=tdem, fill = NA) + #tdem region boundaries with no fill (uses default ouline line weight)
  north(tdem, symbol = 10, location = "bottomright") + #north arrow definied by tdem region layer, poistion in bottom right of plot
  theme_void() + #using ggplot theme "void"
  scale_fill_gradientn(colours = rank_pal, na.value = "#FFFFFF",breaks=c(min(rank$total_rank),max(rank$total_rank)), labels=c("High","Low"))+ #continous scale key: color from rank pelette, set na.value fill; generate break markers at min and max values; label min and max values
  labs(title = "Thunderstorm and Severe Wind Hazard Ranking", #main title
       subtitle = "Based on damage, injuries, fatalities, and events", #subtitle
       caption = "Source: National Center for Environmental Information Storm Events Database\nhttps://ncdc.noaa.gov/stormevents/") + #caption with source name and url
  theme(
    legend.position = "bottom", #set legend at bottom of plot
    legend.title = element_blank(), #remove legend title
    legend.key.width = unit(1.5, 'cm'), #scale key width
    plot.caption.position = "panel", #set the caption (source) position to the panel instead of in the plot
    plot.caption = element_text(size = 8, face = "italic"), #set caption text size and italic
    plot.subtitle = element_text(size = 9, face = "italic") #set subtitle text size and italic
  )
map_rank #plot map

ggsave("figures/thunder_wind/fig97a_exp24_windthund_rank.png", map_rank, height = 5, width = 5) #save figure as 5 in by 5 in png

##### total damage for wind events####
damage <- st_read(db, query="select 
counties.geoid ,
counties.name,
ncei.*,
counties.the_geom
from (select
cz_name,
count(event_id) as count_event,
coalesce (sum(damage_property_adj22),0) as damage_property_adj22,
coalesce (sum(damage_crops_adj22),0) as damage_crops_adj22,
coalesce (sum(damage_total_adj22),0) as damage_total_adj22,
avg(damage_total_adj22)/21  as aal,
count(event_id)/21 as frequency
from ncei_events as ne
where event_type in ('Strong Wind', 'Thunderstorm Wind', 'High Wind')
group by  cz_name
) as ncei
right join counties 
	on
	ncei.cz_name  = upper(counties.name)
;
")

damage <- damage %>% 
  replace_na(list(damage_total_adj22=0)) #na's where counties have no damages to join; replace these with zeros

classIntervals(damage$damage_total_adj22, style = "quantile", dataPrecision = 1, n=6) #generate quantile breaks for total damage field

damage <- damage %>%   #create breaks in dataframe based on intervals (round to make them "prettier")                                               
  mutate(breaks = cut(damage_total_adj22, breaks = c("0","1","400000","800000","1000000","2000000","5000000","564628653"), include.lowest = TRUE))

#choropleth map with breaks =
map_damage <- ggplot() + #map object
  geom_sf(damage, mapping=aes(fill = breaks), color = "#D6D6D6", size=0.2) + #counties: fill based on total damage breaks, set outline color and outline line weight
  geom_sf(data=tdem, fill = NA) + #tdem region boundaries with no fill
  north(tdem, symbol = 10, location = "bottomright") + #set north arrow based on tdem region layer
  theme_void() + # ggplot theme "void"
  scale_color_manual(values =  c(dmg_pal, "#A7A7A7"),na.value="#D6D6D6", aesthetics = "fill",name= "$",labels = c("0", "400K","800K", "1M", "2M", "5M", "600M")) + #set fill colors to dmg_pal, add "$" infront of keys, add labels to each key based on breaks
  guides(fill = guide_legend(keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'left', nrow=1)) + #set key height and width, set key label postion, set legend title ($) postion, force legend to stay one row
  labs(title = "Total Damage due to Thunderstorm and Severe Wind", #plot title
       subtitle = "From 2000-2021", #subtitle
       caption = "Source: National Center for Environmental Information Storm Events Database\nhttps://ncdc.noaa.gov/stormevents/") + #source and url
  theme(
    legend.position = "bottom", #legend position at bottom of plot
    legend.title = element_text(size=10, vjust = 1), #legend title size and adjust vertically to be inline with keys
    legend.spacing.y = unit(1, "mm"), #spacing between keys and labels
    legend.key = element_rect(color ="#A7A7A7" ), #rectangle around keys so white for zeros doesn't blend into background
    plot.caption.position = "panel", #set the caption (source) position to the panel instead of in the plot
    plot.caption = element_text(size = 8, face = "italic"), #set caption text size and italic
    plot.subtitle = element_text(size = 9, face = "italic") #set subtitle text size and italic
  )
map_damage

ggsave("figures/thunder_wind/fig95_exp24_wind_loss.png", map_damage, height = 5, width = 5) #save figure as 5 in by 5 in png
