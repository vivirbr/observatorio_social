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
consolidated <- read.csv("diversasocioambiental/data/consolidated_metrics.csv")

consolidated_aggregated <- consolidated %>% filter(year==2022) %>%
                             mutate(social_risk = rowMeans(select(., freq,terra,trabalhoescravo), na.rm = TRUE),
                                    env_risk = rowMeans(select(., deforestation,embargoes), na.rm = TRUE)) %>%
                             select(CD_MUN,social_risk,env_risk,perc_area)

consolidated_aggregated$social_risk_level <- ifelse(consolidated_aggregated$social_risk <0.10,"LOW",
                                               ifelse(consolidated_aggregated$social_risk >0.25,"HIGH","HIGH"))
consolidated_aggregated$env_risk_level <- ifelse(consolidated_aggregated$env_risk <0.25,"LOW",
                                               ifelse(consolidated_aggregated$env_risk >0.50,"HIGH","HIGH"))

consolidated_aggregated$consolidated_cases <- paste(consolidated_aggregated$social_risk_level,
                                                    consolidated_aggregated$env_risk_level,sep="-")

map_consolidated<- merge(map,consolidated_aggregated,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)


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

png(filename="diversasocioambiental/map/plots/consolidated_levels_2022.png", 
             width = 3000, height = 3000, units = "px", res = 300)
ggplot(
  data = map_consolidated 
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
  scale_fill_manual(values = c("black", "deeppink4", "darkred","gray"),name = "categorical risk")+
#  scale_fill_viridis(
#     option = "H",
#     name = "normalized index",
#     alpha = 0.9, 
#     begin = 0.1, 
#     end = 0.9,
#     discrete = T,
#     direction = -1,
#     na.translate = F,
#     guide = guide_legend(
#      keyheight = unit(5, units = "mm"),
#      title.position = "top",
#      reverse = F
#   )) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Consolidated metric in 2022",
       subtitle = "Based on social and environmental risks",
       caption = "Social risk is based on land and water conflicts and forced labor, while environmental risk is based on deforestation and embargoes of ilegal deforestation") +
  # add theme
  theme_map()
dev.off()




