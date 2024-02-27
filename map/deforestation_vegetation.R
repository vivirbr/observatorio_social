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
alerts <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_alerts_mun_vegetation.csv")
map_deforestation<- merge(map,alerts,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

# write.csv(alerts,"diversasocioambiental/input_tables/mapbiomas_alerts_mun_vegetation.csv",row.names=FALSE)


#------ Parameters -----#

# extract quantiles
quantiles_forest_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
quantiles_forest_2022 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 

quantiles_owl_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
quantiles_owl_2022 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 

quantiles_others_2021 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 
quantiles_others_2022 <- c(0.001,100,1000,15000,35000,75000,100000,150000) 


# here we create custom labels
labels_forest_2021 <- imap_chr(quantiles_forest_2021, function(., idx){
  return(paste0(round(quantiles_forest_2021[idx], 0),
                             " – ",
                             round(quantiles_forest_2021[idx + 1], 0)))
})
labels_forest_2021 <- labels_forest_2021[1:length(labels_forest_2021) - 1]


labels_forest_2022 <- imap_chr(quantiles_forest_2022, function(., idx){
  return(paste0(round(quantiles_forest_2022[idx], 0),
                             " – ",
                             round(quantiles_forest_2022[idx + 1], 0)))
})
labels_forest_2022 <- labels_forest_2022[1:length(labels_forest_2022) - 1]


labels_owl_2021 <- imap_chr(quantiles_owl_2021, function(., idx){
  return(paste0(round(quantiles_owl_2021[idx], 0),
                             " – ",
                             round(quantiles_owl_2021[idx + 1], 0)))
})
labels_owl_2021 <- labels_owl_2021[1:length(labels_owl_2021) - 1]

labels_owl_2022 <- imap_chr(quantiles_owl_2022, function(., idx){
  return(paste0(round(quantiles_owl_2022[idx], 0),
                             " – ",
                             round(quantiles_owl_2022[idx + 1], 0)))
})
labels_owl_2022 <- labels_owl_2022[1:length(labels_owl_2022) - 1]


labels_others_2021 <- imap_chr(quantiles_others_2021, function(., idx){
  return(paste0(round(quantiles_others_2021[idx], 0),
                             " – ",
                             round(quantiles_others_2021[idx + 1], 0)))
})
labels_others_2021 <- labels_others_2021[1:length(labels_others_2021) - 1]

labels_others_2022 <- imap_chr(quantiles_others_2022, function(., idx){
  return(paste0(round(quantiles_others_2022[idx], 0),
                             " – ",
                             round(quantiles_others_2022[idx + 1], 0)))
})
labels_others_2022 <- labels_others_2022[1:length(labels_others_2022) - 1]


map_deforestation %<>%
  mutate(mean_quantiles_forest_2021 = cut(alertsforest_2021,
                               breaks = quantiles_forest_2021,
                               labels = labels_forest_2021,
                               include.lowest = T),
         mean_quantiles_forest_2022 = cut(alertsforest_2022,
                               breaks = quantiles_forest_2022,
                               labels = labels_forest_2022,
                               include.lowest = T),
        mean_quantiles_owl_2021 = cut(alertsowl_2021,
                               breaks = quantiles_owl_2021,
                               labels = labels_owl_2021,
                               include.lowest = T),
        mean_quantiles_owl_2022 = cut(alertsowl_2022,
                               breaks = quantiles_owl_2022,
                               labels = labels_owl_2022,
                               include.lowest = T),
        mean_quantiles_others_2021 = cut(alertsothers_2021,
                               breaks = quantiles_others_2021,
                               labels = labels_others_2021,
                               include.lowest = T),
        mean_quantiles_others_2022 = cut(alertsothers_2022,
                               breaks = quantiles_others_2022,
                               labels = labels_others_2022,
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

png(filename="diversasocioambiental/map/plots/alerts_forest_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_forest_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in forests in 2021",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/alerts_forest_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_forest_2022
      ),
    color = "white",
    size = 0.1
  ) +
 scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in forests in 2022",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/alerts_owl_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_owl_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in other wooded lands in 2021",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +

  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/alerts_owl_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_owl_2022
      ),
    color = "white",
    size = 0.1
  ) +
 scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in other wooded lands in 2022",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +

  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/alerts_others_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_others_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in other vegetations in 2021",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/alerts_others_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
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
      fill = mean_quantiles_others_2022
      ),
    color = "white",
    size = 0.1
  ) +
 scale_fill_viridis(
    option = "turbo",
    name = "Deforestation alers\nin hectares",
    alpha = 0.9, 
    begin = 0.6, 
    end = 1,
    discrete = T,
    na.translate = F,
    direction = 1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Municipality deforestation in other vegetations in 2022",
       subtitle = "As reported by MapBiomas Alerts - harmonized with FAO 2012 definition",
       caption= "When MapBiomas classes were associated to both forest and other wooded lands (OWL), the class assumed was OWL.") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()
