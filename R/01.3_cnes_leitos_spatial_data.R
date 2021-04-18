
# description -------------------------------------------------------------

# this script identifies which hexagon each hospital saved at 01.2 belongs

# steps:
# 1. read dataset saved at 01.2 (hospitals within aop cities, sus and which
#...offer neonatal uti beds)
# 2. subset individual city within aop 
# 3. transform to sf
# 4. read individual hexagon sf for each city
# 5. identify which hexagon each hospital belongs (spatial join)
# (?) correct hospitals that do not fall within hospitals in the ttmattrix
#### repeat process to each city to generate hexagons for each one with number of 
# hospitals and neonatal beds within them
# save dataset

# check:
# https://github.com/ipeaGIT/acesso_covid19/blob/master/R/1-read_hospital_equip.R
# https://github.com/ipeaGIT/acesso_oport/blob/50324b0107a81ff5e9fa6e85389df862c50c1520/R/02.3-agrupamento_das_variaveis_por_hexagonos.R

# setup -------------------------------------------------------------------

source('R/00_setup.R')


# define function ---------------------------------------------------------

funcao <- function(){
  
  # read data
  aop_hospitals <- readr::read_rds('//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais_equipamentos/hospitais_leitos_neonatal_201812.rds')
  # select columns
  aop_hospitals <- aop_hospitals[
    ,
    .(cnes,code_muni,lon,lat,neonatal_exist,neonatal_contr,neonatal_sus,neonatal_nsus)
  ]
  
  aop_hospitals <- aop_hospitals %>% 
    sf::st_as_sf(coords = c('lon','lat'), crs = 4326)
  
}

