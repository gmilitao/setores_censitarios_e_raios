
### Início em 07/03/2023
### script para obter uma circunferência que cubra todo o setor censitário
# usando a base de setores censitários do CENSO 2010, disponível no pacote geobr

# Adaptado da questão no stackoverflow:
# https://stackoverflow.com/questions/60234988/finding-the-radius-of-a-circle-that-circumscribes-a-polygon

library(tidyverse)
library(sf)
library(tmap)
library(geobr)


# download dos polígonos

setores_total <- geobr::read_census_tract(code_tract = "all", simplified = FALSE,
                                          showProgress = T)
# criando amostra de 2295 setores

set.seed(987654)

setores_2 <- sample_n(setores_total, 2295) |> 
  st_transform(3857) |>
  arrange(code_tract)

# calculando os "cascos" ou pontas

casco <- setores_2 |> st_convex_hull()

# computando os centroides
geo_center <- st_centroid(setores_2)

# criando nova base adicionando o centroide a cada ponto dos polígonos
casco_pontos <- casco |>  
  mutate(centroid_geometry = geo_center$geom) |> 
  st_cast("POINT")


# Calculando a distância do centroide em relação a todos os pontos do casco
casco_pontos$dist_to_centroid <- as.numeric(casco_pontos  |>  
                                             st_distance(casco_pontos$centroid_geometry, by_element = TRUE))

# selecionando o ponto mais distante do centroide de cada setor
casco_max <- casco_pontos |> 
  arrange(code_tract) |> 
  group_by(code_tract) |> 
  summarize(max_dist = max(dist_to_centroid)) |>  
  ungroup()

# Desenhar um círculo usando a distância obtida:
geo_circumscribed <- geo_center %>% st_buffer(casco_max$max_dist)

# pegando só setores de São paulo para plotar como exemplo

setores_sao_paulo <- setores_2 |> filter(code_muni==3550308)

casco_SP <- casco |> filter(code_muni==3550308)

geo_center_SP <- geo_center |> filter(code_muni==3550308)
  
geo_circumscribed_SP <- geo_circumscribed |> filter(code_muni==3550308)


# plotando dados de SP como exemplo
tm_shape(setores_sao_paulo) + 
  tm_borders(col = "red") + 
  tm_shape(casco_SP) + 
  tm_borders(col = "blue", alpha = 0.5) + 
  tm_shape(geo_center_SP ) + 
  tm_symbols(col = "red", size = 0.1) + 
  tm_shape(geo_circumscribed_SP) + 
  tm_borders(col = "green")





