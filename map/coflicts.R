library(tidyverse)
library(reshape2)
library(magrittr)
library(viridis)
library(dplyr)  
library(geobr)
library(sf)

options(scipen=999)

#BR map - municipalities
map <- read_municipality(year=2018)
biome <- read_biomes(year=2019) %>% filter(name_biome!="Sistema Costeiro")

#CPT
agua <- read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_agua.csv")
agua_freq <- melt(table(agua$CD_MUN,agua$Ano))
colnames(agua_freq) <- c("CD_MUN","Ano","Freq")

terra <- read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_terra.csv")
terra <- terra %>% group_by(CD_MUN,Ano) %>% summarize(conflitos=sum(Numero.De.Conflitos))

violencia <- read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/violencia_pessoa.csv")
violencia <- violencia %>% group_by(CD_MUN,Ano) %>% summarize(pessoas=sum(Numero.De.Pessoas))

map_agua <- merge(map,agua_freq,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

map_terra <- merge(map,terra,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

map_violencia <- merge(map,violencia,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

#---------------------------#
#----Conflitos por Agua----#
#---------------------------#

#------ Parameters -----#

# extract quantiles
# quantiles <- map_agua %>% filter(Freq!=0) %>%
#   pull(Freq) %>%
#   quantile(probs = c(0, 0.8, 0.95, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles[length(quantiles)]<-max(map_agua$Freq,na.rm=T)

quantiles <- c(1,3,5,8,12)

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]
fix(labels) # manually fixing intervals

map_agua %<>%
  mutate(mean_quantiles = cut(Freq,
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

png(filename="diversasocioambiental/map/plots/conflicts_water_2021.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_agua %>% filter(Ano==2021)
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
  # scale_fill_manual(
  #   values=c("#999999", "#E69F00", "#56B4E9", "#56B4E9"),
  #   name = "Conflicts over water\nnumber of conflicts",
  #   na.translate = F,
  #   guide = guide_legend(
  #    keyheight = unit(5, units = "mm"),
  #    title.position = "top",
  #    reverse = T)) +
  scale_fill_viridis(
    option = "turbo",
    name = "Conflicts over water\nnumber of conflicts",
    alpha = 0.9, 
    begin = 0.7, 
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
       title = "Number of conflicts over water in 2021",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/conflicts_water_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_agua %>% filter(Ano==2022)
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
    name = "Conflicts over water\nnumber of conflicts",
    alpha = 0.9, 
    begin = 0.7, 
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
       title = "Number of conflicts over water in 2022",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

#---------------------------#
#----Conflitos por Terra----#
#---------------------------#

#------ Parameters -----#

# extract quantiles
# quantiles <- map_terra %>% filter(conflitos!=0) %>%
#   pull(conflitos) %>%
#   quantile(probs = c(0, 0.8, 0.95, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles[length(quantiles)]<-max(map_terra$conflitos,na.rm=T)
quantiles <- c(1,5,15,30,43)


# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]
fix(labels)

map_terra %<>%
  mutate(mean_quantiles = cut(conflitos,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

#------- PLOT --------#

png(filename="diversasocioambiental/map/plots/conflicts_land_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_terra %>% filter(Ano==2021)
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
    name = "Conflicts over land\nnumber of conflicts",
    alpha = 0.9, 
    begin = 0.7, 
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
       title = "Number of conflicts over land in 2021",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/conflicts_land_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_terra %>% filter(Ano==2022)
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
    name = "Conflicts over land\nnumber of conflicts",
    alpha = 0.9, 
    begin = 0.7, 
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
       title = "Number of conflicts over land in 2022",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()


#----------------------------------#
#----Violencia contra a pessoa ----#
#----------------------------------#

#------ Parameters -----#

# extract quantiles
# quantiles <- map_violencia %>% filter(pessoas!=0) %>%
#   pull(pessoas) %>%
#   quantile(probs = c(0, 0.5, 0.9, 0.99, 1)) %>%
#   signif(2) %>%
#   as.vector() 
# quantiles[length(quantiles)]<-max(map_violencia$pessoas,na.rm=TRUE)
quantiles<-c(1,5,15,30,45,78)
fix(quantiles)

# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]
fix(labels)

map_violencia %<>%
  mutate(mean_quantiles = cut(pessoas,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

#------- PLOT --------#

png(filename="diversasocioambiental/map/plots/violence_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_violencia %>% filter(Ano == 2021)
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
    name = "Victims of violence\nnumber of people",
    alpha = 0.9, 
    begin = 0.7, 
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
       title = "Violence against people in 2021",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()

png(filename="diversasocioambiental/map/plots/violence_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_violencia %>% filter(Ano == 2022)
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
    name = "Victims of violence\nnumber of people",
    alpha = 0.9, 
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
       title = "Violence against people in 2022",
       subtitle = "As reported by CPT") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 
dev.off()
