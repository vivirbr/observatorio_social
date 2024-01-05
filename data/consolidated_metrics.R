options(scipen=999)
library(tidyverse)
library(caret)


# Goal here is to normalize all metrics from 0 to 1 to consolidate all metrics in a single map

#Deforestation
alerts <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_alerts_mun.csv") %>%
              select(CD_MUN,mapbiomas_alerts_2021,,mapbiomas_alerts_2022)  %>%
              pivot_longer(cols=c('mapbiomas_alerts_2021', 'mapbiomas_alerts_2022'),
                           names_to='year',
                           values_to='deforestation') %>%
              mutate(year=gsub("mapbiomas_alerts_","",year),
                     deforestation=round(deforestation,0))

#CPT
agua <- read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_agua.csv")
agua_freq <- data.frame(table(agua$CD_MUN,agua$Ano)) %>%
             rename(CD_MUN="Var1",year="Var2",freq="Freq")

terra <- read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_terra.csv")
terra <- terra %>% 
              group_by(CD_MUN,Ano) %>% 
              summarize(terra=sum(Numero.De.Conflitos)) %>% 
              rename(year="Ano")

#Forced labor
forcedlabor <- read.csv("diversasocioambiental/data/forced_labor/full_radarsit.csv") %>%
                  pivot_longer(cols=c('qtds_2021_rbind', 'qtds_2022_rbind'),
                                names_to='year',
                                values_to='trabalhoescravo') %>%
                  mutate(year=gsub("qtds_|_rbind","",year))


# Merging all data sources
full<- Reduce(function(...) merge(..., by=c("CD_MUN","year"),all=T),list(alerts,agua_freq,terra,forcedlabor))
full[is.na(full)] <- 0

# Normalizing the data
process <- preProcess(full, method=c("range"))
norm_scale <- predict(process, full)
norm_scale$CD_MUN<-full$CD_MUN #bringing back the correct CD_MUN

norm_scale$consolidated <- rowSums(norm_scale[,3:6]/4)

write.csv(norm_scale,"diversasocioambiental/data/consolidated_metrics.csv",row.names=FALSE)

