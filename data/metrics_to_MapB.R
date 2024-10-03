library(tidyverse)
library(reshape2)

agua<-read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_agua.csv")
terra<-read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/conflitos_terra.csv")
violencia<-read.csv("diversasocioambiental/data/cpt/from_cpt/w_geocodes/violencia_pessoa.csv")
trabalhoescravo<-read.csv("diversasocioambiental/data/forced_labor/full_radarsit.csv")

#improving data structure


agua_freq <- melt(table(agua$CD_MUN,agua$Ano)) %>% 
               rename(CD_MUN="Var1", Ano="Var2", Freq="value") %>% 
               pivot_wider(names_from = Ano, values_from = Freq) %>%
               rename(agua_2021=`2021`,agua_2022=`2022`)
terra_freq <- melt(table(terra$CD_MUN,terra$Ano)) %>% 
               rename(CD_MUN="Var1", Ano="Var2", Freq="value") %>% 
               pivot_wider(names_from = Ano, values_from = Freq) %>%
               rename(terra_2021=`2021`,terra_2022=`2022`)
violencia_freq <- melt(table(pessoa$CD_MUN,pessoa$Ano)) %>% 
               rename(CD_MUN="Var1", Ano="Var2", Freq="value") %>% 
               pivot_wider(names_from = Ano, values_from = Freq) %>%
               rename(violencia_2021=`2021`,violencia_2022=`2022`)
trabalhoescravo <- trabalhoescravo %>%
                     rename(trabalhoescravo_2021=qtds_2021_rbind,trabalhoescravo_2022=qtds_2022_rbind)

full <- list(agua_freq,terra_freq,violencia_freq,trabalhoescravo) %>% reduce(full_join, by="CD_MUN")

write.csv(full,"diversasocioambiental/data/dadosiniciais_observatoriosocial.csv",sep=",",row.names=F)
