library(tidyverse)
library(magrittr)
library(fedmatch)
library(viridis)
library(dplyr)  
library(geobr)
library(sf)

options(scipen=999)

#BR map - municipalities
map <- read_municipality(year=2018)
biome <- read_biomes(year=2019) %>% filter(name_biome!="Sistema Costeiro")

#Forced labor
commodity <- read.csv("/home/vivrbr/Dev/diversasocioambiental/data/ibge/commodity_production.csv") 
commodity <- merge(map,commodity, by="code_muni",all.x=TRUE)

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

#------ Parameters and plot -----#

# extract quantiles

####################
###    SOY       ###
####################

quantiles <- commodity %>% filter(soy!=0) %>%
  pull(soy) %>%
  quantile(probs = c(0.2, 0.5, 0.7, 0.99, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$soy,na.rm=TRUE)
quantiles[1]<-1

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

commodity %<>%
  mutate(mean_quantiles = cut(soy,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/soy_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Area of soy \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted soy in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/soy_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Area of soy \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted soy in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

####################
###    BEEF      ###
####################

quantiles <- commodity %>% filter(beef!=0) %>%
  pull(beef) %>%
  quantile(probs = c(0,0.1, 0.5, 0.7, 0.995, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$beef,na.rm=TRUE)

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]
fix(labels)

commodity %<>%
  mutate(mean_quantiles = cut(beef,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/beef_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Number of \nheads",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Cattle herd size in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/beef_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Number \nof heads",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Cattle herd size in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

####################
###    coffee    ###
####################

quantiles <- commodity %>% filter(coffee!=0) %>%
  pull(coffee) %>%
  quantile(probs = c(0.2, 0.5, 0.7, 0.99, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$coffee,na.rm=TRUE)
quantiles[1]<-1

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

commodity %<>%
  mutate(mean_quantiles = cut(coffee,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/coffee_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Area of coffee \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted coffee in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/coffee_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Area of coffee \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted coffee in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

####################
###    cocoa    ###
####################

quantiles <- commodity %>% filter(cocoa!=0) %>%
  pull(cocoa) %>%
  quantile(probs = c(0.2, 0.5, 0.7, 0.99, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$cocoa,na.rm=TRUE)
quantiles[1]<-1

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

commodity %<>%
  mutate(mean_quantiles = cut(cocoa,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/cocoa_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Area of cocoa \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted cocoa in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/cocoa_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Area of cocoa \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted cocoa in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

####################
###    palm      ###
####################

quantiles <- commodity %>% filter(palm!=0) %>%
  pull(palm) %>%
  quantile(probs = c(0.2, 0.5, 0.7, 0.99, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$palm,na.rm=TRUE)
quantiles[1]<-1

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

commodity %<>%
  mutate(mean_quantiles = cut(palm,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/palm_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Area of palm \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted palm in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/palm_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Area of palm \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted palm in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

####################
###    rubber    ###
####################

quantiles <- commodity %>% filter(rubber!=0) %>%
  pull(rubber) %>%
  quantile(probs = c(0.2, 0.5, 0.7, 0.99, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(commodity$rubber,na.rm=TRUE)
quantiles[1]<-1

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

commodity %<>%
  mutate(mean_quantiles = cut(rubber,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

png(filename="diversasocioambiental/map/plots/rubber_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2021)
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
    option = "turbo",
    name = "Area of rubber \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted for rubber in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/rubber_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = commodity %>% filter(year==2022)
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
    option = "turbo",
    name = "Area of rubber \nin hectares",
    alpha = 0.7, 
    begin = 0.7, 
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
       title = "Area of planted for rubber in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()