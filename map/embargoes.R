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

#Forced labor
embargoes <- read.csv("diversasocioambiental/data/embargoes/rel_areas_embargadas_0-82104_2024-01-04_073207.csv") %>%
                mutate(ano=str_sub(DAT_EMBARGO,7,10))

filtered <- embargoes %>% 
             mutate(QTD_AREA_EMBARGADA = gsub("(.*),.*", "\\1", embargoes$QTD_AREA_EMBARGADA),
                    QTD_AREA_DESMATADA = gsub("(.*),.*", "\\1", embargoes$QTD_AREA_DESMATADA)
             )

embargo_2022 <- filtered %>% 
              filter(ano==2022) %>%
              select(MUNICIPIO,QTD_AREA_DESMATADA,QTD_AREA_EMBARGADA)


# Adding the IBGE geocodes to the dataset
mun_br <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_deforestation_mun.csv") %>% 
                   mutate(id2 = as.character(seq(1:nrow(.)))) %>% 
                   select(id2,NM_MUN,SIGLA_UF,CD_MUN) %>%
                   mutate(UF_MUN = paste(SIGLA_UF,NM_MUN,sep="_"))

embargo_2022_unique_mun <-  data.frame(MUNICIPIO = unique(embargo_2022$MUNICIPIO),
                                        id1 = seq(1:length(unique(embargo_2022$MUNICIPIO))))

embargo_2022_join <- merge_plus(embargo_2022_unique_mun, mun_br, 
                                by.x='MUNICIPIO',
                                by.y='NM_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

embargo_2022_join_full <- plyr::rbind.fill(embargo_2022_join$matches,embargo_2022_join$data1_nomatch)

# Fixing no matches manually 
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==30,1100346,embargo_2022_join_full$CD_MUN)
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==62,2400208,embargo_2022_join_full$CD_MUN)
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==206,3122900,embargo_2022_join_full$CD_MUN)
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==389,2922250,embargo_2022_join_full$CD_MUN)
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==509,5107008,embargo_2022_join_full$CD_MUN)
embargo_2022_join_full$CD_MUN<-ifelse(embargo_2022_join_full$id1==580,5107800,embargo_2022_join_full$CD_MUN)


# Assembling all parts
embargo_final <- merge(embargo_2022,embargo_2022_join_full %>% select(MUNICIPIO,CD_MUN),by="MUNICIPIO",all.x=TRUE)



embargo_final <- data.frame(table(embargo_final$CD_MUN))
colnames(embargo_final)<-c("CD_MUN","freq")
#write.csv(embargo_final,"diversasocioambiental/data/embargoes/embargo_w_geocode.csv",row.names=FALSE)


map_embargo<- merge(map,embargo_final,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)


#------ Parameters -----#

# extract quantiles
quantiles <- map_embargo %>% filter(freq!=0) %>%
  pull(freq) %>%
  quantile(probs = c(0, 0.7, 0.9, 0.95, 1)) %>%
  signif(2) %>%
  as.vector() 
quantiles[length(quantiles)]<-max(map_embargo$freq,na.rm=TRUE)


# here we create custom labels
labels <- imap_chr(quantiles, function(., idx){
  return(paste0(round(quantiles[idx], 0),
                             " – ",
                             round(quantiles[idx + 1], 0)))
})
labels <- labels[1:length(labels) - 1]

map_embargo %<>%
  mutate(mean_quantiles = cut(freq,
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
    name = "Number\nof embargoes",
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
       title = "Deforestation embargoes",
       subtitle = "As reported by IBAMA",
       caption = "") +
  # add theme
  theme_map()
dev.off()


