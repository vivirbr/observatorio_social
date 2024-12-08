library(tidyverse)

emb_desm <- read.csv('Fern/areas_embargadas_desm.csv')
emb_outros <- read.csv('Fern/areas_embargadas_outros.csv')

emb_desm$ano <- str_sub(emb_desm$DAT_TAD,7,10)
emb_outros$ano <- str_sub(emb_outros$DAT_TAD,7,10)

emb_desm_g <- emb_desm %>% group_by(ano,COD_MUNICIPIO) %>% count(COD_MUNICIPIO) %>% add_column(tipo="embargos_desmatamento")
emb_outros_g <- emb_outros %>% group_by(ano,COD_MUNICIPIO) %>% count(COD_MUNICIPIO) %>% add_column(tipo="embargos_outros")

embargo_full <- rbind(emb_desm_g,emb_outros_g)

colnames(embargo_full)<-c("Ano","Geocodigo","Valor","Tipo")

write.csv(embargo_full,'Fern/embargos.csv',row.names=F)
