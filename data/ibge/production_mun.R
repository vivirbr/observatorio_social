options(scipen=999)
library(tidyverse)
library(sidrar)


# commodities listed
# Table 1612: soy 
# Table 3939: beef 
# Table 1613: coffee, cocoa, palm, rubber

# Missing timber 

soy<-get_sidra(api = "/t/1612/n6/all/v/109/p/last%202/c81/0,2713") #ha
soy_f1 <- soy %>% 
            filter(`Produto das lavouras temporárias` != "Total") %>%
            select(`Município (Código)`,Ano,Valor) %>%
            rename(soy=Valor)
soy_f1[is.na(soy_f1)] <- 0

cattle_herd<-get_sidra(api = "/t/3939/n6/all/v/all/p/last%202/c79/2670") #heads
cattle_f1 <- cattle_herd %>% 
                select(`Município (Código)`,Ano,Valor) %>%
            rename(beef=Valor)
cattle_f1[is.na(cattle_f1)] <- 0

coffee<-get_sidra(api = "/t/1613/n6/all/v/2313/p/last%202/c82/2723")
coffee_f1 <- coffee %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(coffee=Valor)
coffee_f1[is.na(coffee_f1)] <- 0

cocoa<-get_sidra(api = "/t/1613/n6/all/v/2313/p/last%202/c82/2722")
cocoa_f1 <- cocoa %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(cocoa=Valor)
cocoa_f1[is.na(cocoa_f1)] <- 0

palm<-get_sidra(api = "/t/1613/n6/all/v/2313/p/last%202/c82/2728")
palm_f1 <- palm %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(palm=Valor)
palm_f1[is.na(palm_f1)] <- 0

rubber<-get_sidra(api = "/t/1613/n6/all/v/2313/p/last%202/c82/2721")
rubber_f1 <- rubber %>% 
                filter(`Produto das lavouras permanentes` != "Total") %>%
                select(`Município (Código)`,Ano,Valor) %>%
            rename(rubber=Valor)
rubber_f1[is.na(rubber_f1)] <- 0

## Bringing all of them together
full<- Reduce(function(...) merge(..., by=c("Município (Código)","Ano"),all=T),list(soy_f1,cattle_f1,coffee_f1,cocoa_f1,rubber_f1,palm_f1))
full <- full %>% rename(code_muni=`Município (Código)`,
                        year= Ano)

# For any weird reason prop.table is not working thats why I will run it by year
full_2021 <- full %>% filter(year==2021) %>% mutate(soy_perc = soy/sum(soy,na.rm=TRUE),
                                                    beef_perc = beef/sum(beef,na.rm=TRUE),
                                                    coffee_perc = coffee/sum(coffee,na.rm=TRUE),
                                                    cocoa_perc = cocoa/sum(cocoa,na.rm=TRUE),
                                                    rubber_perc = rubber/sum(rubber,na.rm=TRUE),
                                                    palm_perc = palm/sum(palm,na.rm=TRUE))
full_2022 <- full %>% filter(year==2022) %>% mutate(soy_perc = soy/sum(soy,na.rm=TRUE),
                                                    beef_perc = beef/sum(beef,na.rm=TRUE),
                                                    coffee_perc = coffee/sum(coffee,na.rm=TRUE),
                                                    cocoa_perc = cocoa/sum(cocoa,na.rm=TRUE),
                                                    rubber_perc = rubber/sum(rubber,na.rm=TRUE),
                                                    palm_perc = palm/sum(palm,na.rm=TRUE))
full_perc <- rbind(full_2021,full_2022)

write.csv(full_perc,"diversasocioambiental/data/ibge/commodity_production.csv",row.names=FALSE)
