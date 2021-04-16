
# description -------------------------------------------------------------

# this script adds to aop_hospitals data columns containing the number of
# neonatal beds (total, sus, nsus).

# steps:
# 1. read raw data (aop_hospitals and hospitals beds data saved by script #1)
# 2. filter data
# 2.1 filter aop cities within beds data
# 2.2 filter aop hospitals (sus) within beds data
# 2.3 filter beds by type (neonatal) within beds data
# 3 create dt with number of beds by hospitals (cnes)
# 4 left join beds to aop_hospitals data
# 5 save resulting data

# setup -------------------------------------------------------------------

source('./R/00_setup.R')


# define function ---------------------------------------------------------

filter_cnes_beds <- function(){
  
  # 1 read files ------------------------------------------------------------
  
  
  # * 1.1 read hospitals beds data (saved at scrpit #1) ---------------
  
  # read hospitals beds data (saved at script #1)
  beds <- data.table::fread(
    input = '../../data-raw/hospitais_equipamentos/cnes_let_201812.csv',
    colClasses = 'character'
  ) %>% 
    janitor::clean_names()
  
  # check data
  #dplyr::glimpse(beds)
  
  
  # * 1.2 read aop hospitals data -------------------------------------------
  
  # read data 
  aop_hospitals <- readr::read_rds('//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais/2018/hospitais_filter_geocoded_pmaq_2018.rds') %>% 
    janitor::clean_names()
  

  # REMOVE LAT LON MISSING FROM HOSPITALS DATA  -----------------------------
  # do this here until acess_oport project is updated (which will do this process)
  # remove lat lon missing
  aop_hospitals <- aop_hospitals[!is.na(lat),] 
  
  # filter only estabs with high quality geocode
  table(aop_hospitals$precision_depth)
  
  aop_hospitals <- aop_hospitals[(precision_depth %in% c('cnes','3 Estrelas', '4 Estrelas', 
                                      'airport', 'amusement_park', 'PMAQ', 
                                      'bus_station', 'establishment',
                                      'intersection', 'neighborhood', 
                                      'political', 'post_box', 'street_number',
                                      'premise', 'subpremise',
                                      'town_square', 'postal_code'))]
  
  # 2 filter data -----------------------------------------------------------
  
  
  # * 2.1 filter aop cities within beds data --------------------------------
  
  # code municipality with 6 characters to match beds df 
  code_muni6 <- substr(x = munis_df$code_muni, start = 1, stop = 6)
  
  # filter aop cities
  beds_aop_cities <- beds[
    .(code_muni6), 
    on = c('codufmun'),
    nomatch = NULL
  ]
  
  # * 2.2 filter aop hospitals (sus) within beds data -----------------------
  
  # filter aop hospitals
  beds_aop_hosp <- beds_aop_cities[
    .(aop_hospitals$cnes), 
    on = c('cnes'),
    nomatch = NULL
  ]
  
  
  # * 2.3 filter beds by type (neonatal) ------------------------------------
  
  # filter neonatal uti beds
  beds_neonatal <- beds_aop_hosp[
    .(c('80','81','82','92','93')),
    on = c('codleito'),
    nomatch = NULL
  ]
  
  # columns to change type
  qt_cols <- c('qt_exist','qt_contr','qt_sus','qt_nsus')
  
  # change type of columns containing the quantity of beds
  beds_neonatal[
    , 
    (qt_cols) := lapply(.SD, as.integer), 
    .SDcols = qt_cols
  ]
  
  # number of beds by muni
  # sus sum
  beds_neonatal[, .(qtd_sus = sum(qt_sus, na.rm = T))]
  # total by muni
  #beds_neonatal[, .(qtd_exist = sum(qt_exist, na.rm = T)), by = .(codufmun)]
  # sus by muni
  #beds_neonatal[, .(qtd_sus = sum(qt_sus, na.rm = T)), by = .(codufmun)]
  
  
  # 3 create dt with number of beds by hospital -----------------------------
  
  # create dt with number of neonatal beds offered by each hospital by cnes
  #cnes_beds_sus <- beds_neonatal[, .(neonatal_beds_sus = sum(qt_sus)), by = .(cnes)]
  cnes_beds <- beds_neonatal[
    , 
    lapply(.SD, sum, na.rm = T),
    by = .(cnes),
    .SDcols = qt_cols
  ]
  
  # check total
  sum(cnes_beds$qt_sus)
  
  # change columns names
  setnames(cnes_beds, 
           c("qt_exist","qt_contr","qt_sus","qt_nsus"),
           gsub('qt','neonatal',colnames(cnes_beds[, !('cnes')]))
  )
  
  
  # 4 left join beds to aop hospitals data ----------------------------------
  
  # add number of neonatal beds to aop_hospitals data
  aop_hospitals[
    cnes_beds,
    `:=` (
      neonatal_exist = i.neonatal_exist,
      neonatal_contr = i.neonatal_contr,
      neonatal_sus = i.neonatal_sus,
      neonatal_nsus = i.neonatal_nsus
    ),
    on = "cnes"
  ]
  
  # vector with neonatal_cols
  neonatal_cols <- c("neonatal_exist","neonatal_contr","neonatal_sus","neonatal_nsus")
  
  # replace neonatal_beds NA with 0
  aop_hospitals[
    ,
    (neonatal_cols) := lapply(.SD, tidyr::replace_na, replace = 0),
    .SDcols = neonatal_cols
  ]
  
  
  # 5 save data -------------------------------------------------------------
  
  # select columns or save whole data?
  readr::write_rds(
    x = aop_hospitals, 
    file = '//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais_equipamentos/hospitais_leitos_neonatal_201812.rds',
    compress = 'gz')
  
}


# run function ------------------------------------------------------------
filter_cnes_beds()



