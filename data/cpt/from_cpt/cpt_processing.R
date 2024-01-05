library(tidyverse)
library(fedmatch)

#need to bring the ibge geocodes to the cpt data
mun_br <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_deforestation_mun.csv") %>% 
                   mutate(id2 = as.character(seq(1:nrow(.)))) %>% 
                   select(id2,NM_MUN,SIGLA_UF,CD_MUN) %>%
                   mutate(UF_MUN = paste(SIGLA_UF,NM_MUN,sep="_"))

#---------------------------#
#----Conflitos por Terra----#
#---------------------------#
terra <- read.csv("diversasocioambiental/data/cpt/from_cpt/conflitos_terra_2014_2023_1_semestre.csv") %>%
            filter(Ano>2020 & Ano < 2023) %>%
            select(Ano,Uf.Sigla,Municipio.Primario,Numero.De.Conflitos) 

terra$Municipio.Primario <- gsub("\\s*\\([^\\)]+\\)","",terra$Municipio.Primario)

terra <- terra %>% mutate(municipio = paste(Uf.Sigla,Municipio.Primario,sep="_"))

terra_unique_mun <-  data.frame(municipio = unique(terra$municipio),
                     id1 = seq(1:length(unique(terra$municipio))))

terra_join <- merge_plus(terra_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

terra_join_full <- plyr::rbind.fill(terra_join$matches,terra_join$data1_nomatch)

# Fixing no matches manually 
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==79,5107800,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==448,3522158,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==496,4216057,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==547,2922250,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==712,4315503,terra_join_full$CD_MUN)

# Assembling all parts
terra_final <- merge(terra,terra_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)

write.csv(terra_final,"diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_terra.csv",row.names=FALSE)

#---------------------------#
#----Conflitos por Agua----#
#---------------------------#
agua <- read.csv("diversasocioambiental/data/cpt/from_cpt/conflitos_agua_2014_2023_1_semestre.csv") %>%
            filter(Ano>2020 & Ano < 2023) %>%
            select(Ano,Uf.Sigla,Municipio.Primario) 

agua$Municipio.Primario <- gsub("\\s*\\([^\\)]+\\)","",agua$Municipio.Primario)

agua <- agua %>% mutate(municipio = paste(Uf.Sigla,Municipio.Primario,sep="_"))

agua_unique_mun <-  data.frame(municipio = unique(agua$municipio),
                     id1 = seq(1:length(unique(agua$municipio))))

agua_join <- merge_plus(agua_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

agua_join_full <- plyr::rbind.fill(agua_join$matches,agua_join$data1_nomatch)

# Fixing no matches manually 
agua_join_full$CD_MUN<-ifelse(agua_join_full$id1==85,5105622,agua_join_full$CD_MUN)

# Assembling all parts
agua_final <- merge(agua,agua_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)

write.csv(agua_final,"diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_agua.csv",row.names=FALSE)

#----------------------------------#
#----Violencia contra a pessoa ----#
#----------------------------------#

pessoa <- read.csv("/home/vivrbr/Dev/diversasocioambiental/data/cpt/from_cpt/violencia_contra_a_pessoa_2014-2023_1_semestre.csv") %>%
            filter(Ano>2020 & Ano < 2023) %>%
            select(Ano,Uf.Sigla,Municipio.Primario,Numero.De.Pessoas,Categoria.Vitima.Violencia,Eixos.De.Violencia) 


pessoa$Municipio.Primario <- gsub("\\s*\\([^\\)]+\\)","",pessoa$Municipio.Primario)

pessoa <- pessoa %>% mutate(municipio = paste(Uf.Sigla,Municipio.Primario,sep="_"))

pessoa_unique_mun <-  data.frame(municipio = unique(pessoa$municipio),
                     id1 = seq(1:length(unique(pessoa$municipio))))

pessoa_join <- merge_plus(pessoa_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

pessoa_join_full <- plyr::rbind.fill(pessoa_join$matches,pessoa_join$data1_nomatch)

# Fixing no matches manually 
pessoa_join_full$CD_MUN<-ifelse(pessoa_join_full$id1==62,4216057,pessoa_join_full$CD_MUN)
pessoa_join_full$CD_MUN<-ifelse(pessoa_join_full$id1==86,5105622,pessoa_join_full$CD_MUN)

# Assembling all parts
pessoa_final <- merge(pessoa,pessoa_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)

write.csv(pessoa_final,"diversasocioambiental/data/cpt/from_cpt/w_geocodes/violencia_pessoa.csv",row.names=FALSE)

