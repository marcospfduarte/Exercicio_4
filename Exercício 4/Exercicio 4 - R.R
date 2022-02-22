######### EXERCÍCIO 4 #######
### Georefenenciamento###
### pacotes e pacman necessários
pacman::p_load(terra, spData)
library(dplyr)
library(geobr)
library(ggplot2)
library(sf)


# importando os dados 
dados = rast("brasil_coverage_2020.tif")
plot(dados) ### Plotando os dados refentes a todo o brasil

#dados relativos aos municioios pelo pacote geobr

mun = geobr::read_municipality(year =2020)
sao_paulo = mun %>% 
  filter(abbrev_state == "SP") 

# crop, mask e extract
cr = crop(dados, sao_paulo)
ms = mask(cr, vect(sao_paulo))

plot(cr)
plot(ms)



## Coberturas 
ex = extract(ms, vect(sao_paulo))
cob_total <- ex %>% #cobertura total
  group_by(ID) %>%
  summarise(cobertura = n())

cob_vegetal <- ex %>% #cobertura vegeral
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1,3,4,5,49)) %>%
  summarise(cobertura_v = n())

cob_sp <- merge(cob_total, cob_vegetal, by=c("ID"), all.x=TRUE) #juntando as coberturas

cob_sp <- cob_sp %>%
  mutate(p_v = cobertura_v/cob_total) #cobertura vegetal / cobertura total

#criando coluna ID no df original e juntando
sp <- sao_paulo %>%
  mutate(ID = c(1:645), .after = code_muni) %>%
  left_join(cob_sp, by = "ID")

#Mapa
sp$cobertura_tentativa_2 <- sp$cobertura_v/sp$cobertura

grafico_cob_sp <- sp %>%
  ggplot() +
  geom_sf(aes(fill = cobertura_tentativa_2), alpha = 5, col = "white") +
  scale_fill_viridis_c(name = "Porcentagem") +
  labs(title = "Porcentagem de Cobertura Vegetal", subtitle = "São Paulo - Estado")
grafico_cob_sp

#municípios com cobertura vegetal
sp_v <- sp %>%
  subset(select = c(name_muni, cobertura_tentativa_2))

write.csv(sp_v, "tabela.csv")

