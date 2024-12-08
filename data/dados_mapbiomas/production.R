options(scipen=999)
library(tidyverse)
library(sidrar)

# commodities listed
# Table 1612: soy 
# Table 3939: beef 
# Table 1613: coffee, cocoa, palm, rubber

# Missing timber 
soy<-list()
for(year in 2000:2024){
soy[[year]]<-get_sidra(api = paste0("/t/1612/n6/all/v/109/p/",year,"/c81/0,2713")) #ha
Sys.sleep(1)
print(paste0("soy ",year))
}
soy_f0 <- do.call("rbind",soy)
soy_f1 <- soy_f0 %>% 
            filter(`Produto das lavouras temporárias` != "Total") %>%
            select(`Município (Código)`,Ano,Valor) %>%
            rename(soy=Valor)
soy_f1[is.na(soy_f1)] <- 0
soy_f1 <- soy_f1 %>% add_column(Tipo="Soja")
colnames(soy_f1) <- c("Geocodigo","Ano","Valor","Tipo")

coffee<-list()
for(year in 2019:2024){
coffee[[year]]<-get_sidra(api = paste0("/t/1613/n6/all/v/2313/p/",year,"/c82/2723")) #ha
Sys.sleep(1)
print(paste0("coffee ",year))
}
coffee_f0 <- do.call("rbind",coffee)
coffee_f1 <- coffee_f0 %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(coffee=Valor)
coffee_f1[is.na(coffee_f1)] <- 0
coffee_f1 <- coffee_f1 %>% add_column(Tipo="Cafe")
colnames(coffee_f1) <- c("Geocodigo","Ano","Valor","Tipo")

save.image(file='myEnvironment.RData')
cocoa<-list()
for(year in 1990:2024){
cocoa[[year]]<-get_sidra(api = paste0("/t/1613/n6/all/v/2313/p/",year,"/c82/2722")) #ha
Sys.sleep(1)
print(paste0("cocoa ",year))
}
cocoa_f0 <- do.call("rbind",cocoa)
cocoa_f1 <- cocoa_f0 %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(cocoa=Valor)
cocoa_f1[is.na(cocoa_f1)] <- 0
cocoa_f1 <- cocoa_f1 %>% add_column(Tipo="Cacau")
colnames(cocoa_f1) <- c("Geocodigo","Ano","Valor","Tipo")

palm<-list()
for(year in 1990:2024){
palm[[year]]<-get_sidra(api = paste0("/t/1613/n6/all/v/2313/p/",year,"/c82/2728")) #ha
Sys.sleep(1)
print(paste0("palm ",year))
}
palm_f0 <- do.call("rbind",palm)
palm_f1 <- palm_f0 %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(palm=Valor)
palm_f1[is.na(palm_f1)] <- 0
palm_f1 <- palm_f1 %>% add_column(Tipo="Palma")
colnames(palm_f1) <- c("Geocodigo","Ano","Valor","Tipo")


rubber<-list()
for(year in 1990:2024){
rubber[[year]]<-get_sidra(api = paste0("/t/1613/n6/all/v/2313/p/",year,"/c82/2721")) #ha
Sys.sleep(1)
print(paste0("rubber ",year))
}
rubber_f0 <- do.call("rbind",rubber)
rubber_f1 <- rubber_f0 %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(rubber=Valor)
rubber_f1[is.na(rubber_f1)] <- 0
rubber_f1 <- rubber_f1 %>% add_column(Tipo="Borracha")
colnames(rubber_f1) <- c("Geocodigo","Ano","Valor","Tipo")

cattle_herd<-list()
for(year in 2009:2024){
cattle_herd[[year]]<-get_sidra(api = paste0("/t/3939/n6/all/v/all/p/",year,"/c79/2670")) #ha
Sys.sleep(1)
print(year)
}
cattle_f0 <- do.call("rbind",cattle_herd)
cattle_f1 <- cattle_f0 %>% 
                select(`Município (Código)`,Ano,Valor) %>%
            rename(beef=Valor)
cattle_f1[is.na(cattle_f1)] <- 0
cattle_f1 <- cattle_f1 %>% add_column(Tipo="Bovino")
colnames(cattle_f1) <- c("Geocodigo","Ano","Valor","Tipo")

## Bringing all of them together
full<- do.call("rbind",list(soy_f1,cocoa_f1,coffee_f1,rubber_f1,palm_f1,cattle_f1))
write.csv(full,'Fern/producao_1990_2024.csv',row.names=F)

