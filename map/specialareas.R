library(tidyverse)
library(magrittr)
library(viridis)
library(dplyr)  
library(geobr)
library(sf)

options(scipen=999)

#BR map - municipalities
map <- read_municipality(year=2018)
biome <- read_biomes(year=2019) %>% filter(name_biome!="Sistema Costeiro")

#Deforestation
areas <- read.csv("diversasocioambiental/data/special_areas/areas_especiais.csv") %>% 
              select(-system.index,-.geo) %>%
              mutate(perc_area = ((sum/100)/AREA_KM2)*100)

map_areas<- merge(map,areas,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

#------ Parameters -----#
#  quantile(probs = c(0,seq(0.8, 1, length.out = no_classes + 1))) %>%

# extract quantiles
quantiles_areas <- c(0,5,25,50,75,90,100) 

# here we create custom labels
labels_areas <- imap_chr(quantiles_areas, function(., idx){
  return(paste0(round(quantiles_areas[idx], 0),
                             " – ",
                             round(quantiles_areas[idx + 1], 0)))
})
labels_areas <- labels_areas[1:length(labels_areas) - 1]
fix(labels_areas)

map_areas %<>%
  mutate(quantiles_areas = cut(perc_area,
                               breaks = quantiles_areas,
                               labels = labels_areas,
                               include.lowest = T))


#------- PLOT --------#

theme_map <- function(...) {
  theme_minimal() +
  theme(
    text = element_text(color = default_font_color),
    legend.position = c(0.22, 0.25),
    # remove all axes
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    # add a subtle grid
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # background colors
    plot.background = element_rect(fill = default_background_color,
                                   color = NA),
    panel.background = element_rect(fill = default_background_color,
                                    color = NA),
    legend.background = element_rect(fill = default_background_color,
                                     color = NA),
    # borders and margins
    plot.margin = unit(c(.5, .5, .2, .5), "cm"),
    panel.border = element_blank(),
    panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
    # titles
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10, hjust = 0,
                               color = default_font_color),
    plot.title = element_text(size = 20, hjust = 0.5,
                              color = default_font_color),
    plot.subtitle = element_text(size = 15, hjust = 0.5,
                                 color = default_font_color,
                                 margin = margin(b = -0.1,
                                                 t = -0.1,
                                                 l = 2,
                                                 unit = "cm"),
                                 debug = F),
    # captions
    plot.caption = element_text(size = 7,
                                hjust = .5,
                                margin = margin(t = 0.2,
                                                b = 0,
                                                unit = "cm"),
                                color = "#939184"),
    ...
  )
}

default_font_color<-"#000000"
default_background_color<-"transparent"

png(filename="diversasocioambiental/map/plots/special_areas.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_areas
    ) +
  geom_sf(
    data = map,
    fill = "#d1d1d1",
    color = "white"
  ) +
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + 
  geom_sf(
    mapping = aes(
      fill = quantiles_areas
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "viridis",
    name = "Percentage of\nspecial areas",
    alpha = 0.9, 
    begin = 0.2, 
    end = 1,
    discrete = T,
    direction = 1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Existence of special areas in the municipality",
       subtitle = "Conservation units, indigenous lands, settlements and Quilombola lands",
       caption = "Conservation units does not include the class Área de Protecão Ambiental (APA)") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

