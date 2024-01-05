library(tidyverse)
library(magrittr)
library(viridis)
library(dplyr)  
library(geobr)
library(sf)

options(scipen=999)

#BR map - municipalities
map <- read_municipality(year=2018)

#Forced labor
forcedlabor <- read.csv("diversasocioambiental/data/forced_labor/full_radarsit.csv")

map_forcedlabor<- merge(map,forcedlabor,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

#------ Parameters -----#

# extract quantiles
quantiles_2021 <- map_forcedlabor %>% filter(qtds_2021_rbind!=0) %>%
  pull(qtds_2021_rbind) %>%
  quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles_2021[length(quantiles_2021)]<-max(map_forcedlabor$qtds_2021_rbind)

quantiles_2022 <- map_forcedlabor %>% filter(qtds_2022_rbind!=0) %>%
  pull(qtds_2022_rbind) %>%
  quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles_2022[length(quantiles_2022)]<-500

# here we create custom labels
labels_2021 <- imap_chr(quantiles_2021, function(., idx){
  return(paste0(round(quantiles_2021[idx], 0),
                             " – ",
                             round(quantiles_2021[idx + 1], 0)))
})
labels_2021 <- labels_2021[1:length(labels_2021) - 1]

labels_2022 <- imap_chr(quantiles_2022, function(., idx){
  return(paste0(round(quantiles_2022[idx], 0),
                             " – ",
                             round(quantiles_2022[idx + 1], 0)))
})
labels_2022 <- labels_2022[1:length(labels_2022) - 1]

map_forcedlabor %<>%
  mutate(mean_quantiles_2021 = cut(qtds_2021_rbind,
                               breaks = quantiles_2021,
                               labels = labels_2021,
                               include.lowest = T),
         mean_quantiles_2022 = cut(qtds_2022_rbind,
                               breaks = quantiles_2022,
                               labels = labels_2022,
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

png(filename="diversasocioambiental/map/plots/forced_labor_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_forcedlabor
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
      fill = mean_quantiles_2021
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Forced labor\nnumber of people",
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
       title = "Number of people in 2021 found working in conditions that are similar to slave labor",
       subtitle = "As reported by Radar SIT") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/forced_labor_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_forcedlabor
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
      fill = mean_quantiles_2022
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "Forced labor\nnumber of people",
    alpha = 0.9, 
    begin = 0.1, 
    end = 0.9,
    discrete = T,
    na.translate = F,
    direction = -1,
    guide = guide_legend(
     keyheight = unit(5, units = "mm"),
     title.position = "top",
     reverse = T
  )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Number of people in 2022 found working in conditions that are similar to slave labor",
       subtitle = "As reported by Radar SIT") +
  # add theme
  theme_map()
dev.off()