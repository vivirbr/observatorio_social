library(tidyverse)
library(magrittr)
library(viridis)
library(dplyr)  
library(geobr)
library(sf)

options(scipen=999)

#BR map - municipalities
map <- read_municipality(year=2018)

#Deforestation
alerts <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_alerts_mun.csv")
prodes <- read.csv("diversasocioambiental/data/deforestation/prodes_deforestation_mun.csv")
mapbiomas <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_deforestation_mun.csv")

map_deforestation<- merge(map,prodes,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)
map_deforestation<- merge(map_deforestation,alerts,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)
map_deforestation<- merge(map_deforestation,mapbiomas,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

#------ Parameters -----#
#  quantile(probs = c(0,seq(0.8, 1, length.out = no_classes + 1))) %>%

# extract quantiles
quantiles_alerts_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
# <- map_deforestation %>% filter(mapbiomas_alerts_2021!=0) %>%
#   pull(mapbiomas_alerts_2021) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles_alerts_2021[length(quantiles_alerts_2021)]<-max(map_deforestation$mapbiomas_alerts_2021)

quantiles_alerts_2022 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
# map_deforestation %>% filter(mapbiomas_alerts_2022!=0) %>%
#   pull(mapbiomas_alerts_2022) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles_alerts_2022[length(quantiles_alerts_2022)]<-max(map_deforestation$mapbiomas_alerts_2022)

quantiles_prodes_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
# map_deforestation %>% filter(prodes_deforestation_2021!=0) %>%
#   pull(prodes_deforestation_2021) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles_prodes_2021[length(quantiles_prodes_2021)]<-max(map_deforestation$prodes_deforestation_2021)

quantiles_prodes_2022 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
# map_deforestation %>% filter(prodes_deforestation_2022!=0) %>%
#   pull(prodes_deforestation_2022) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles_prodes_2022[length(quantiles_prodes_2022)]<-max(map_deforestation$prodes_deforestation_2022)

quantiles_mapbiomas_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
# map_deforestation %>% filter(mapbiomas_deforestation_2021!=0) %>%
#   pull(mapbiomas_deforestation_2021) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles_mapbiomas_2021[length(quantiles_mapbiomas_2021)]<-max(map_deforestation$mapbiomas_deforestation_2021)

# here we create custom labels
labels_alerts_2021 <- imap_chr(quantiles_alerts_2021, function(., idx){
  return(paste0(round(quantiles_alerts_2021[idx], 0),
                             " – ",
                             round(quantiles_alerts_2021[idx + 1], 0)))
})
labels_alerts_2021 <- labels_alerts_2021[1:length(labels_alerts_2021) - 1]


labels_alerts_2022 <- imap_chr(quantiles_alerts_2022, function(., idx){
  return(paste0(round(quantiles_alerts_2022[idx], 0),
                             " – ",
                             round(quantiles_alerts_2022[idx + 1], 0)))
})
labels_alerts_2022 <- labels_alerts_2022[1:length(labels_alerts_2022) - 1]

labels_prodes_2021 <- imap_chr(quantiles_prodes_2021, function(., idx){
  return(paste0(round(quantiles_prodes_2021[idx], 0),
                             " – ",
                             round(quantiles_prodes_2021[idx + 1], 0)))
})
labels_prodes_2021 <- labels_prodes_2021[1:length(labels_prodes_2021) - 1]

labels_prodes_2022 <- imap_chr(quantiles_prodes_2022, function(., idx){
  return(paste0(round(quantiles_prodes_2022[idx], 0),
                             " – ",
                             round(quantiles_prodes_2022[idx + 1], 0)))
})
labels_prodes_2022 <- labels_prodes_2022[1:length(labels_prodes_2022) - 1]


labels_mapbiomas_2021 <- imap_chr(quantiles_mapbiomas_2021, function(., idx){
  return(paste0(round(quantiles_mapbiomas_2021[idx], 0),
                             " – ",
                             round(quantiles_mapbiomas_2021[idx + 1], 0)))
})
labels_mapbiomas_2021 <- labels_mapbiomas_2021[1:length(labels_mapbiomas_2021) - 1]

map_deforestation %<>%
  mutate(mean_quantiles_alerts_2021 = cut(mapbiomas_alerts_2021,
                               breaks = quantiles_alerts_2021,
                               labels = labels_alerts_2021,
                               include.lowest = T),
         mean_quantiles_alerts_2022 = cut(mapbiomas_alerts_2022,
                               breaks = quantiles_alerts_2022,
                               labels = labels_alerts_2022,
                               include.lowest = T),
        mean_quantiles_prodes_2021 = cut(prodes_deforestation_2021,
                               breaks = quantiles_prodes_2021,
                               labels = labels_prodes_2021,
                               include.lowest = T),
        mean_quantiles_prodes_2022 = cut(prodes_deforestation_2022,
                               breaks = quantiles_prodes_2022,
                               labels = labels_prodes_2022,
                               include.lowest = T),
        mean_quantiles_mapbiomas_2021 = cut(mapbiomas_deforestation_2021,
                               breaks = quantiles_mapbiomas_2021,
                               labels = labels_mapbiomas_2021,
                               include.lowest = T)
            )

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

png(filename="diversasocioambiental/map/plots/deforestation_alerts_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_deforestation
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
      fill = mean_quantiles_alerts_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Area deforested\nin hectares",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    direction = -1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in 2021",
       subtitle = "As reported by MapBiomas Alerts") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/deforestation_alerts_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_deforestation
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
      fill = mean_quantiles_alerts_2022
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Area deforested\nin hectares",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    direction = -1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in 2022",
       subtitle = "As reported by MapBiomas Alerts") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/deforestation_prodes_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_deforestation
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
      fill = mean_quantiles_prodes_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Area deforested\nin hectares",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    direction = -1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in 2021",
       subtitle = "As reported by Prodes/Inpe") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/deforestation_prodes_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_deforestation
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
      fill = mean_quantiles_prodes_2022
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Area deforested\nin hectares",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    direction = -1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in 2022",
       subtitle = "As reported by Prodes/Inpe") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/deforestation_mapbiomas_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_deforestation
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
      fill = mean_quantiles_mapbiomas_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Area deforested\nin hectares",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    direction = -1,
    na.translate = F,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in 2021 ",
       subtitle = "As reported by MapBiomas annual deforestation",
       caption = "Only deforestation in primary vegetation was captured") +
  # add theme
  theme_map()
dev.off()
