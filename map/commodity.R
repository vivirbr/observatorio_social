library(tidyverse)
library(magrittr)
library(fedmatch)
library(viridis)
library(dplyr)  
library(geobr)
library(grid)
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

## adding to the plot the calculation based on the consolidated areas
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
consolidated_aggregated_2021$social_risk_level <- ifelse(consolidated_aggregated_2021$social_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2021$social_risk >0 & consolidated_aggregated_2021$social_risk < 0.031,"LOW","HIGH"))

consolidated_aggregated_2021$env_risk_level <- ifelse(consolidated_aggregated_2021$env_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2021$env_risk >0 & consolidated_aggregated_2021$env_risk < 0.0028,"LOW","HIGH"))

consolidated_aggregated_2021$consolidated_cases <- paste(consolidated_aggregated_2021$social_risk_level,
                                                    consolidated_aggregated_2021$env_risk_level,sep="-")

consolidated_aggregated_2021 <- consolidated_aggregated_2021 %>%
                                  mutate(consolidated_cases = case_when(consolidated_cases=="HIGH-HIGH" ~ "High reported socioenvironmental impacts",
                                                                        consolidated_cases=="HIGH-LOW"|
                                                                        consolidated_cases=="HIGH-NO"|
                                                                        consolidated_cases=="LOW-HIGH"|
                                                                        consolidated_cases=="NO-HIGH" ~ "Medium reported socioenvironmental impacts",
                                                                        consolidated_cases=="LOW-LOW"|
                                                                        consolidated_cases=="LOW-NO"|
                                                                        consolidated_cases=="NO-LOW" ~ "Low reported socioenvironmental impacts",
                                                                        consolidated_cases=="NO-NO" ~ "No information on socioenvironmental impacts"))

# 2022
consolidated_aggregated_2022$social_risk_level <- ifelse(consolidated_aggregated_2022$social_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2022$social_risk >0 & consolidated_aggregated_2022$social_risk < 0.031,"LOW","HIGH"))

consolidated_aggregated_2022$env_risk_level <- ifelse(consolidated_aggregated_2022$env_risk == 0,"NO",
                                               ifelse(consolidated_aggregated_2022$env_risk >0 & consolidated_aggregated_2022$env_risk < 0.0023,"LOW","HIGH"))

consolidated_aggregated_2022$consolidated_cases <- paste(consolidated_aggregated_2022$social_risk_level,
                                                    consolidated_aggregated_2022$env_risk_level,sep="-")

consolidated_aggregated_2022 <- consolidated_aggregated_2022 %>%
                                  mutate(consolidated_cases = case_when(consolidated_cases=="HIGH-HIGH" ~ "High reported socioenvironmental impacts",
                                                                        consolidated_cases=="HIGH-LOW"|
                                                                        consolidated_cases=="HIGH-NO"|
                                                                        consolidated_cases=="LOW-HIGH"|
                                                                        consolidated_cases=="NO-HIGH" ~ "Medium reported socioenvironmental impacts",
                                                                        consolidated_cases=="LOW-LOW"|
                                                                        consolidated_cases=="LOW-NO"|
                                                                        consolidated_cases=="NO-LOW" ~ "Low reported socioenvironmental impacts",
                                                                        consolidated_cases=="NO-NO" ~ "No information on socioenvironmental impacts"))

## Merging the commodity file with the classification
classes_2021 <- merge(commodity %>% filter(year==2021),consolidated_aggregated_2021,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)
classes_2022 <- merge(commodity %>% filter(year==2022),consolidated_aggregated_2022,by.x="code_muni",by.y="CD_MUN",all.x=TRUE)

chart_table_2021 <- data.frame(classes_2021) %>% 
                        group_by(consolidated_cases) %>% 
                        summarize(soy_risk=sum(soy_perc,na.rm=TRUE)*100,
                                  beef_risk=sum(beef_perc,na.rm=TRUE)*100,
                                  coffee_risk=sum(coffee_perc,na.rm=TRUE)*100,
                                  cocoa_risk=sum(cocoa_perc,na.rm=TRUE)*100,
                                  palm_risk=sum(palm_perc,na.rm=TRUE)*100,
                                  rubber_risk=sum(rubber_perc,na.rm=TRUE)*100)
chart_table_2022 <- data.frame(classes_2022) %>% 
                        group_by(consolidated_cases) %>% 
                        summarize(soy_risk=sum(soy_perc,na.rm=TRUE)*100,
                                  beef_risk=sum(beef_perc,na.rm=TRUE)*100,
                                  coffee_risk=sum(coffee_perc,na.rm=TRUE)*100,
                                  cocoa_risk=sum(cocoa_perc,na.rm=TRUE)*100,
                                  palm_risk=sum(palm_perc,na.rm=TRUE)*100,
                                  rubber_risk=sum(rubber_perc,na.rm=TRUE)*100)

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

## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=soy_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 45))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=soy_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 45))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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

commodity %<>%
  mutate(mean_quantiles = cut(beef,
                               breaks = quantiles,
                               labels = labels,
                               include.lowest = T))

## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=beef_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 45))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=beef_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 45))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
    name = "Cattle herd \nin heads",
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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
    name = "Cattle herd \nin heads",
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
       title = "Cattle herd size in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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

## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=coffee_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 60))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=coffee_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 60))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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

## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=cocoa_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 70))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=cocoa_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 70))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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

## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=palm_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 60))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=palm_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 60))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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


## commodity chart
chart_2021<-ggplot(chart_table_2021, aes(x=consolidated_cases, y=rubber_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 65))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")
chart_2022<-ggplot(chart_table_2022, aes(x=consolidated_cases, y=rubber_risk, fill=consolidated_cases)) +
          geom_bar(stat="identity")+
          scale_y_continuous(limits = c(0, 65))+
          scale_x_discrete(limits=c("No information on socioenvironmental impacts",
                                    "Low reported socioenvironmental impacts",
                                    "Medium reported socioenvironmental impacts",
                                    "High reported socioenvironmental impacts"))+
          scale_fill_manual(values=c("#540b0e","#e09f3e","#9e2a2b","gray"))+
          theme(panel.background = element_rect(fill = "white",
                                colour = "white",
                                size = 0.5, linetype = "solid"),
                axis.title.x=element_blank(),
                axis.text.x=element_blank()) + 
          labs(fill = "Social environmental impact",
                y ="Percentage of production")

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
    name = "Area used for rubber \nin hectares",
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
       title = "Area of used for rubber in 2021",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) + 
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2021),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
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
    name = "Area used for rubber \nin hectares",
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
       title = "Area used for rubber in 2022",
       subtitle = "As reported by IBGE",
       caption = "") +
  # add theme
  theme_map()+
  theme(legend.position="top")+
  geom_sf(
    data = biome,
    fill = "transparent",
    color = "gray30"
  ) +
  scale_x_continuous(limits = c(-75, -30))+
  scale_y_continuous(limits = c(3, -40))+
  annotation_custom(
    grob = ggplotGrob(chart_2022),
    xmin = -70,
    xmax = -30,
    ymin = -42,
    ymax = -35
  ) 
dev.off()

