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

# Condolidated
consolidated <- read.csv("diversasocioambiental/data/consolidated_metrics.csv")

consolidated_aggregated_2021 <- consolidated %>% filter(year==2021) %>%
                             mutate(social_risk = rowMeans(select(., freq,terra,trabalhoescravo), na.rm = TRUE),
                                    env_risk = rowMeans(select(., deforestation), na.rm = TRUE)) %>%
                             select(CD_MUN,social_risk,env_risk,perc_area)
consolidated_aggregated_2022 <- consolidated %>% filter(year==2022) %>%
                             mutate(social_risk = rowMeans(select(., freq,terra,trabalhoescravo), na.rm = TRUE),
                                    env_risk = rowMeans(select(., deforestation,embargoes), na.rm = TRUE)) %>%
                             select(CD_MUN,social_risk,env_risk,perc_area)

# 2021
summary(consolidated_aggregated_2021$env_risk[consolidated_aggregated_2021$env_risk!=0])
summary(consolidated_aggregated_2021$social_risk[consolidated_aggregated_2021$social_risk!=0])
# social risk low was defined by the 3rd quartile without 0 values - or 0.031 
# env risk low was defined by the median without 0 values - or 0.0028 

consolidated_aggregated_2021$social_risk_level <- ifelse(consolidated_aggregated_2021$social_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2021$social_risk >0 & consolidated_aggregated_2021$social_risk < 0.031,"LOW","HIGH"))

consolidated_aggregated_2021$env_risk_level <- ifelse(consolidated_aggregated_2021$env_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2021$env_risk >0 & consolidated_aggregated_2021$env_risk < 0.0028,"LOW","HIGH"))

consolidated_aggregated_2021$consolidated_cases <- paste(consolidated_aggregated_2021$social_risk_level,
                                                    consolidated_aggregated_2021$env_risk_level,sep="-")

map_consolidated_2021<- merge(map,consolidated_aggregated_2021,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

# 2022
summary(consolidated_aggregated_2022$env_risk[consolidated_aggregated_2022$env_risk!=0])
summary(consolidated_aggregated_2022$social_risk[consolidated_aggregated_2022$social_risk!=0])
# social risk low was defined by the 3rd quartile without 0 values - or 0.031
# env risk low was defined by the median without 0 values - or 0.0023

consolidated_aggregated_2022$social_risk_level <- ifelse(consolidated_aggregated_2022$social_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2022$social_risk >0 & consolidated_aggregated_2022$social_risk < 0.031,"LOW","HIGH"))

consolidated_aggregated_2022$env_risk_level <- ifelse(consolidated_aggregated_2022$env_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2022$env_risk >0 & consolidated_aggregated_2022$env_risk < 0.0023,"LOW","HIGH"))

consolidated_aggregated_2022$consolidated_cases <- paste(consolidated_aggregated_2022$social_risk_level,
                                                    consolidated_aggregated_2022$env_risk_level,sep="-")

map_consolidated_2022<- merge(map,consolidated_aggregated_2022,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)


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

png(filename="diversasocioambiental/map/plots/consolidated_levels_2021.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_consolidated_2021 
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
      fill = consolidated_cases
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_manual(values = c("#540b0e", #HH
                               "#ec9a9a", #HL
                               "#ec9a9a", #HN
                               "#ec9a9a", #LH
                               "#fff3b0", #LL
                               "#fff3b0", #LN
                               "#ec9a9a", #NH
                               "#fff3b0", #NL
                               "gray"),name = "categorical risk")+ 
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Consolidated metric in 2021",
       subtitle = "Based on social and environmental risks",
       caption = "Social risk is based on land and water conflicts and forced labor, while environmental risk is based on deforestation alerts") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 

dev.off()



png(filename="diversasocioambiental/map/plots/consolidated_levels_2022.png", 
             width = 3500, height = 3500, units = "px", res = 300)
ggplot(
  data = map_consolidated_2022
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
      fill = consolidated_cases
      ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_manual(values = c("#540b0e", #HH
                               "#ec9a9a", #HL
                               "#ec9a9a", #HN
                               "#ec9a9a", #LH
                               "#fff3b0", #LL
                               "#fff3b0", #LN
                               "#ec9a9a", #NH
                               "#fff3b0", #NL
                               "gray"),name = "categorical risk")+ 
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Consolidated metric in 2022",
       subtitle = "Based on social and environmental risks",
       caption = "Social risk is based on land and water conflicts and forced labor, while environmental risk is based on deforestation and embargoes of ilegal deforestation") +
  # add theme
  theme_map()+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) 

dev.off()




