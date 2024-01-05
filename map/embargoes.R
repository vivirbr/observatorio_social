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
embargoes <- read.csv("diversasocioambiental/data/embargoes/areas_embargadas.csv") %>%
                mutate(ano=str_sub(DAT_EMBARGO,7,10))

filtered <- embargoes[grep("desmatamento",embargoes$DES_INFRACAO),]

filtered <- filtered %>% 
             mutate(QTD_AREA_EMBARGADA = gsub("(.*),.*", "\\1", filtered$QTD_AREA_EMBARGADA),
                    QTD_AREA_DESMATADA = gsub("(.*),.*", "\\1", filtered$QTD_AREA_DESMATADA)
             )


## THERE IS NO DATA HERE
# embargo_2021 <- filtered %>% 
#               filter(ano==2021) %>%
#               select(COD_MUNICIPIO,QTD_AREA_DESMATADA,QTD_AREA_EMBARGADA) %>%
#               group_by(COD_MUNICIPIO) %>%
#               summarize(area_desmatada = sum(as.numeric(QTD_AREA_DESMATADA)),
#                         area_embargada = sum(as.numeric(QTD_AREA_EMBARGADA)))
# embargo_2022 <- filtered %>% 
#               filter(ano==2022) %>%
#               select(COD_MUNICIPIO,QTD_AREA_DESMATADA,QTD_AREA_EMBARGADA) %>%
#               group_by(COD_MUNICIPIO) %>%
#               summarize(area_desmatada = sum(as.numeric(QTD_AREA_DESMATADA)),
#                         area_embargada = sum(as.numeric(QTD_AREA_EMBARGADA)))

embargo_all <- filtered %>% 
                select(COD_MUNICIPIO,QTD_AREA_DESMATADA,QTD_AREA_EMBARGADA) %>%
                group_by(COD_MUNICIPIO) %>%
                summarize(area_desmatada = sum(as.numeric(QTD_AREA_DESMATADA),na.rm=TRUE),
                            area_embargada = sum(as.numeric(QTD_AREA_EMBARGADA),na.rm=TRUE))

map_embargo<- merge(map,embargo_all,by.x="code_muni",by.y="COD_MUNICIPIO",all.x=TRUE)

#------ Parameters -----#

# extract quantiles
quantiles <- map_embargo %>% filter(area_desmatada!=0) %>%
  pull(area_desmatada) %>%
  quantile(probs = c(0, 0.5, 0.9, 0.99, 0.995, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(map_embargo$area_desmatada,na.rm=TRUE)


# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

map_embargo %<>%
  mutate(mean_quantiles = cut(area_desmatada,
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

png(filename="diversasocioambiental/map/plots/embargoes_deforestation.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_embargo
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
       title = "Embargoed areas of illegal deforestation",
       subtitle = "As reported by IBAMA",
       caption = "Data captures embargoes from 2001 to 2013") +
  # add theme
  theme_map()
dev.off()


