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
consolidated <- read.csv("diversasocioambiental/data/consolidated_metrics.csv") %>% 
                select(CD_MUN,year,consolidated) 

map_consolidated<- merge(map,consolidated,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

#------ Parameters -----#

# extract quantiles
quantiles <- map_consolidated %>% filter(consolidated!=0) %>%
  pull(consolidated) %>%
  quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(map_embargo$consolidated,na.rm=TRUE)

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 10),
                             " – ",
                             round(quantiles[idx + 1], 10)))
})
labels <- labels[1:length(labels) - 1]

map_consolidated %<>%
  mutate(mean_quantiles = cut(consolidated,
                               breaks = quantiles,
                               labels = labels,
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

png(filename="diversasocioambiental/map/plots/consolidated_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_consolidated %>% filter(year==2021)
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
      fill = mean_quantiles
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "normalized index",
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
       title = "Consolidated metric in 2021",
       subtitle = "Based on alerts of deforestation, forced labor, water and people violence",
       caption = "") +
  # add theme
  theme_map()
dev.off()

png(filename="diversasocioambiental/map/plots/consolidated_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_consolidated %>% filter(year==2022)
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
      fill = mean_quantiles
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_viridis(
    option = "rocket",
    name = "normalized index",
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
       title = "Consolidated metric in 2022",
       subtitle = "Based on alerts of deforestation, forced labor, water and people violence",
       caption = "") +
  # add theme
  theme_map()
dev.off()


