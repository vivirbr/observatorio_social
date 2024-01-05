# Scraping RadarSIT data
library(tidyverse)
library(httr)

mun_br <- read.csv("diversasocioambiental/data/deforestation/mapbiomas_alerts_mun.csv") %>% select(CD_MUN)

qtds_2021 <- list()
for(i in 1:nrow(mun_br)){
  Sys.sleep(0.5)
  html <- GET(paste0("https://sit.trabalho.gov.br/pentaho/api/repos/:public:SIT:dash:trab_escravo:trab_esc5.xaction/generatedContent?situacao=todas&ano=2021&escravo=encontrado&uf=todos&mun=",mun_br[i,1],"&titulo_ano=2021&titulo_uf=no%20Brasil&cnae=todas"))
  text <- ProTrackR::rawToCharNull(html$content)
  qtds_2021[[i]]<-as.numeric(gsub("'","",str_match(text, ", qtd: \\s*(.*?)\\s*, qtd2:"))[,2])
  print(paste0(i," ok"))
}

qtds_2022 <- list()
for(i in 1:nrow(mun_br)){
  Sys.sleep(0.5)
  html <- GET(paste0("https://sit.trabalho.gov.br/pentaho/api/repos/:public:SIT:dash:trab_escravo:trab_esc5.xaction/generatedContent?situacao=todas&ano=2022&escravo=encontrado&uf=todos&mun=",mun_br[i,1],"&titulo_ano=2022&titulo_uf=no%20Brasil&cnae=todas"))
  text <- ProTrackR::rawToCharNull(html$content)
  qtds_2022[[i]]<-as.numeric(gsub("'","",str_match(text, ", qtd: \\s*(.*?)\\s*, qtd2:"))[,2])
  print(paste0(i," ok"))
}

qtds_2021_rbind <- do.call("rbind",qtds_2021)
qtds_2022_rbind <- do.call("rbind",qtds_2022)

qtds_2021_rbind[is.na(qtds_2021_rbind)] <- 0
qtds_2022_rbind[is.na(qtds_2022_rbind)] <- 0

full_radarsit <- data.frame(mun_br,qtds_2021_rbind,qtds_2022_rbind)

write.csv(full_radarsit,"diversasocioambiental/data/forced_labor/full_radarsit.csv",row.names=FALSE)
