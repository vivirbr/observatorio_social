library(tidyverse)
library(data.table)

desm <- read.csv("Fern/desmatamento_annual_1988_2024.csv")
desm_filtered <- desm[,21:59] %>% select(-classification_1986)

desm_long <- melt(setDT(desm_filtered), id.vars = "TRASE_ID", variable.name = "year")

desm_long$year <- gsub("classification_","",desm_long$year)
desm_long$TRASE_ID <- gsub("BR-","",desm_long$TRASE_ID)
desm_long$value <- round(desm_long$value,0)
desm_long <- desm_long %>% add_column(Tipo="desmatamento_primario")

colnames(desm_long)<-c("Geocodigo","Ano","Valor","Tipo")

write.csv(desm_long,'Fern/desmatamento_primario_1987_2023.csv',row.names=F)
