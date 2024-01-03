library(tidyverse)
library(fedmatch)

#need to bring the ibge geocodes to the cpt data
mun_br <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_deforestation_mun.csv") %>% 
                   mutate(id2 = as.character(seq(1:nrow(.)))) %>% 
                   select(id2,NM_MUN,SIGLA_UF,CD_MUN) %>%
                   mutate(UF_MUN = paste(SIGLA_UF,NM_MUN,sep="_"))

#---------------------------#
#----Conflitos por terra----#
#---------------------------#
terra_2021 <- read.csv("diversasocioambiental/data/cpt/processed/conflitos_2021_processed.csv") %>% add_column(ano=2021) %>% select(-Nome.do.Conflito)
terra_2022 <- read.csv("diversasocioambiental/data/cpt/processed/conflitos_2022_processed.csv") %>% add_column(ano=2022) %>% select(-Nome.do.Conflito)
colnames(terra_2021)<-colnames(terra_2022)
terra<-rbind(terra_2021,terra_2022)

terra$Estado<-mgsub::mgsub(terra$Estado,
c("Acre","Alagoas","Amapá","Amapa","Amazonas","Bahia","Ceará","Ceara","Espírito Santo","Espirito Santo","Goiás","Goias","Maranhão","Maranhao","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Para","Paraíba","Paraiba","Paraná","Parana","Pernambuco","Piauí","Piaui","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Rondonia","Roraima","Santa Catarina","São Paulo","Sao Paulo","Sergipe","Tocantins","Distrito Federal"),
c("AC","AL","AP","AP","AM","BA","CE","CE","ES","ES","GO","GO","MA","MA","MT","MS","MG","PA","PA","PB","PB","PR","PR","PE","PI","PI","RJ","RN","RS","RO","RO","RR","SC","SP","SP","SE","TO","DF"))

terra <- terra %>% add_column(municipio=paste(terra$Estado,terra$Município.s.,sep="_"))

terra_unique_mun <-  data.frame(municipio = unique(terra$municipio),
                     id1 = seq(1:length(unique(terra$municipio))))

terra_join <- merge_plus(terra_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

terra_join_full <- plyr::rbind.fill(terra_join$matches,terra_join$data1_nomatch)

# Fixing no matches manually 
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==215,5106422,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==418,4216057,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==430,3522158,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==510,2922250,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==563,5107800,terra_join_full$CD_MUN)
terra_join_full$CD_MUN<-ifelse(terra_join_full$id1==651,4315503,terra_join_full$CD_MUN)

# Assembling all parts
terra_final <- merge(terra,terra_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)
write.csv(terra_final,"diversasocioambiental/data/cpt/processed/w_geocodes/conflitos_terra.csv",row.names=FALSE)

#---------------------------#
#----Conflitos por Agua----#
#---------------------------#
agua_2021 <- read.csv("diversasocioambiental/data/cpt/processed/agua_2021_processed.csv") %>% add_column(ano=2021) %>% select(-Nome.do.Conflito)
agua_2022 <- read.csv("diversasocioambiental/data/cpt/processed/agua_2022_processed.csv") %>% add_column(ano=2022) %>% select(-Nome.do.Conflito)
colnames(agua_2021)<-colnames(agua_2022)
agua<-rbind(agua_2021,agua_2022)

agua$Estado<-mgsub::mgsub(agua$Estado,
c("Acre","Alagoas","Amapá","Amapa","Amazonas","Bahia","Ceará","Ceara","Espírito Santo","Espirito Santo","Goiás","Goias","Maranhão","Maranhao","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Para","Paraíba","Paraiba","Paraná","Parana","Pernambuco","Piauí","Piaui","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Rondonia","Roraima","Santa Catarina","São Paulo","Sao Paulo","Sergipe","Tocantins","Distrito Federal"),
c("AC","AL","AP","AP","AM","BA","CE","CE","ES","ES","GO","GO","MA","MA","MT","MS","MG","PA","PA","PB","PB","PR","PR","PE","PI","PI","RJ","RN","RS","RO","RO","RR","SC","SP","SP","SE","TO","DF"))

agua <- agua %>% add_column(municipio=paste(agua$Estado,agua$Município.s.,sep="_"))

agua_unique_mun <-  data.frame(municipio = unique(agua$municipio),
                     id1 = seq(1:length(unique(agua$municipio))))

agua_join <- merge_plus(agua_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

agua_join_full <- plyr::rbind.fill(agua_join$matches,agua_join$data1_nomatch)

# Fixing no matches manually 
agua_join_full$CD_MUN<-ifelse(agua_join_full$id1==69,5105622,agua_join_full$CD_MUN)
agua_join_full$CD_MUN<-ifelse(agua_join_full$id1==138,1100015,agua_join_full$CD_MUN)

# Assembling all parts
agua_final <- merge(agua,agua_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)
write.csv(agua_final,"diversasocioambiental/data/cpt/processed/w_geocodes/conflitos_agua.csv",row.names=FALSE)


#---------------------------#
#---- Ocupacao de Terra ----#
#---------------------------#

ocupacao_2021 <- read.csv("diversasocioambiental/data/cpt/processed/ocupacao_2021_processed.csv") %>% add_column(ano=2021) %>% select(-Nome.do.Conflito)
ocupacao_2022 <- read.csv("diversasocioambiental/data/cpt/processed/ocupacao_2022_processed.csv") %>% add_column(ano=2022) %>% select(-Nome.do.Conflito)
colnames(ocupacao_2021)<-colnames(ocupacao_2022)
ocupacao<-rbind(ocupacao_2021,ocupacao_2022)

ocupacao$Estado<-mgsub::mgsub(ocupacao$Estado,
c("Acre","Alagoas","Amapá","Amapa","Amazonas","Bahia","Ceará","Ceara","Espírito Santo","Espirito Santo","Goiás","Goias","Maranhão","Maranhao","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Para","Paraíba","Paraiba","Paraná","Parana","Pernambuco","Piauí","Piaui","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Rondonia","Roraima","Santa Catarina","São Paulo","Sao Paulo","Sergipe","Tocantins","Distrito Federal"),
c("AC","AL","AP","AP","AM","BA","CE","CE","ES","ES","GO","GO","MA","MA","MT","MS","MG","PA","PA","PB","PB","PR","PR","PE","PI","PI","RJ","RN","RS","RO","RO","RR","SC","SP","SP","SE","TO","DF"))

ocupacao <- ocupacao %>% add_column(municipio=paste(ocupacao$Estado,ocupacao$Município.s.,sep="_"))

ocupacao_unique_mun <-  data.frame(municipio = unique(ocupacao$municipio),
                     id1 = seq(1:length(unique(ocupacao$municipio))))

ocupacao_join <- merge_plus(ocupacao_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

ocupacao_join_full <- plyr::rbind.fill(ocupacao_join$matches,ocupacao_join$data1_nomatch)

# Assembling all parts
ocupacao_final <- merge(ocupacao,ocupacao_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)
write.csv(ocupacao_final,"diversasocioambiental/data/cpt/processed/w_geocodes/ocupacao_terra.csv",row.names=FALSE)

#---------------------------#
#---- Trabalho Escravo -----#
#---------------------------#

escravo_2021 <- read.csv("diversasocioambiental/data/cpt/processed/escravo_2021_processed.csv") %>% add_column(ano=2021) %>% select(-Nome.do.Conflito,-Tipo.de.trabalho)
escravo_2022 <- read.csv("diversasocioambiental/data/cpt/processed/escravo_2022_processed.csv") %>% add_column(ano=2022) %>% select(-Nome.do.Conflito,-Tipo.de.Trabalho)
colnames(escravo_2021)<-colnames(escravo_2022)
escravo<-rbind(escravo_2021,escravo_2022)

escravo$Estado<-mgsub::mgsub(escravo$Estado,
c("Acre","Alagoas","Amapá","Amapa","Amazonas","Bahia","Ceará","Ceara","Espírito Santo","Espirito Santo","Goiás","Goias","Maranhão","Maranhao","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Para","Paraíba","Paraiba","Paraná","Parana","Pernambuco","Piauí","Piaui","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Rondonia","Roraima","Santa Catarina","São Paulo","Sao Paulo","Sergipe","Tocantins","Distrito Federal"),
c("AC","AL","AP","AP","AM","BA","CE","CE","ES","ES","GO","GO","MA","MA","MT","MS","MG","PA","PA","PB","PB","PR","PR","PE","PI","PI","RJ","RN","RS","RO","RO","RR","SC","SP","SP","SE","TO","DF"))

escravo <- escravo %>% add_column(municipio=paste(escravo$Estado,escravo$Município.s.,sep="_"))

escravo_unique_mun <-  data.frame(municipio = unique(escravo$municipio),
                     id1 = seq(1:length(unique(escravo$municipio))))

escravo_join <- merge_plus(escravo_unique_mun, mun_br, 
                                by.x='municipio',
                                by.y='UF_MUN',
                                unique_key_1 = "id1",
                                unique_key_2 = "id2")

escravo_join_full <- plyr::rbind.fill(escravo_join$matches,escravo_join$data1_nomatch)

# Assembling all parts
escravo_final <- merge(escravo,escravo_join_full %>% select(municipio,CD_MUN),by="municipio",all.x=TRUE)
write.csv(escravo_final,"diversasocioambiental/data/cpt/processed/w_geocodes/trabalho_escravo.csv",row.names=FALSE)

