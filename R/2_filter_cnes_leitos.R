
# description -------------------------------------------------------------

# this script 
# 1. read raw data of hospital beds saved by script #1
# 2. filter cities within aop framework
# 3. filter hospitals within those cities (onlye SUS (AOP) or private too?)
# 4. filter beds by type
# 5. save resulting data.table

######## OBS CONVERT EVERYTHING TO ONE FUNCTION

# setup -------------------------------------------------------------------

source('./R/0_setup.R')

# 1 read files ------------------------------------------------------------

leitos <- data.table::fread(
  input = '../../data-raw/hospitais_equipamentos/cnes_let_201812.csv',
  colClasses = 'character'
  ) %>% 
  janitor::clean_names()

# check data
dplyr::glimpse(leitos)


# 2 filter AOP cities -----------------------------------------------------

# code municipality with 6 characters to match leitos df 
#code_muni6 <- as.integer(substr(x = munis_df$code_muni, start = 1, stop = 6))
code_muni6 <- substr(x = munis_df$code_muni, start = 1, stop = 6)

# filter aop cities
# CHANGE OBJECT NAME WHEN CONVERTING TO ONE FUNCITON
leitos_aop_cities <- leitos[.(code_muni6), on = c('codufmun')]


# 3 read aop hospitals ----------------------------------------------------

# read data and clean column names
aop_hospitals <- readr::read_rds('//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais/2018/hospitais_filter_geocoded_pmaq_2018.rds') %>% 
  janitor::clean_names()

# filter aop hospitals
leitos_aop_hosp <- leitos_aop_cities[.(aop_hospitals$cnes), on = c('cnes')]


# 4 filter beds by type ---------------------------------------------------

# filter neonatal uti beds
leitos <- subset(leitos_aop_hosp, codleito %in% c('80','81','82','92','93'))

# columns to change type
qt_cols <- c('qt_exist','qt_contr','qt_sus','qt_nsus')

# change type of quantity of beds columns
leitos[
  , 
  (qt_cols) := lapply(.SD, as.integer), 
  .SDcols = qt_cols]

# total number of beds by muni
leitos[, .(qtd_exist = sum(qt_exist)), by = .(codufmun)]
leitos[, .(qtd_sus = sum(qt_sus)), by = .(codufmun)]

#
cnes_leitos <- leitos[, .(qtd_sus = sum(qt_sus)), by = .(cnes)]
sum(cnes_leitos$qtd_sus)



# 5 check information competence ------------------------------------------

# CHECAR SE PASSOS ABAIXO SAO NECESSARIOS

# check types of competence
leitos_aop_hosp %>% count(competen)

# filter NA competence (? CHECAR SE PRECISA)
leitos_aop_hosp_sem_na <- leitos_aop_hosp[!is.na(competen)]

# 6 save data.table -------------------------------------------------------



