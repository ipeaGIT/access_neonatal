
# description -------------------------------------------------------------

# this script 
# 1. read raw data of hospital beds saved by script #1
# 2. filter cities within aop framework
# 3. filter hospitals within those cities (only SUS)
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
leitos_aop_cities <- leitos[
  .(code_muni6), 
  on = c('codufmun'),
  nomatch = NULL
  ]


# 3 read aop hospitals ----------------------------------------------------

# read data and clean column names
aop_hospitals <- readr::read_rds('//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais/2018/hospitais_filter_geocoded_pmaq_2018.rds') %>% 
  janitor::clean_names()

# filter aop hospitals
leitos_aop_hosp <- leitos_aop_cities[
  .(aop_hospitals$cnes), 
  on = c('cnes'),
  nomatch = NULL
  ]


# 4 filter beds by type ---------------------------------------------------

# filter neonatal uti beds
#leitos_neonatal <- subset(leitos_aop_hosp, codleito %in% c('80','81','82','92','93'))
leitos_neonatal <- leitos_aop_hosp[
  .(c('80','81','82','92','93')),
  on = c('codleito'),
  nomatch = NULL
]

# columns to change type
qt_cols <- c('qt_exist','qt_contr','qt_sus','qt_nsus')

# change type of quantity of beds columns
leitos_neonatal[
  , 
  (qt_cols) := lapply(.SD, as.integer), 
  .SDcols = qt_cols
  ]

# total number of beds by muni
#leitos_neonatal[, .(qtd_exist = sum(qt_exist, na.rm = T)), by = .(codufmun)]
#leitos_neonatal[, .(qtd_sus = sum(qt_sus, na.rm = T)), by = .(codufmun)]

# create dt with number of neonatal beds offered by each hospital (by cnes)
#cnes_leitos_sus <- leitos_neonatal[, .(neonatal_beds_sus = sum(qt_sus)), by = .(cnes)]
cnes_leitos <- leitos_neonatal[
  , 
  lapply(.SD, sum, na.rm = T),
  by = .(cnes),
  .SDcols = qt_cols
  ]
sum(cnes_leitos$qt_sus)

# change columns names
setnames(cnes_leitos, 
         c("qt_exist","qt_contr","qt_sus","qt_nsus"),
         gsub('qt','neonatal',colnames(cnes_leitos[, !('cnes')]))
         )

# 5 merge beds and hospitals data -----------------------------------------

# add number of neonatal beds to aop_hospitals data
aop_hospitals[
  cnes_leitos,
  `:=` (
    neonatal_exist = i.neonatal_exist,
    neonatal_contr = i.neonatal_contr,
    neonatal_sus = i.neonatal_sus,
    neonatal_nsus = i.neonatal_nsus
  ),
  on = "cnes"
]

#
neonatal_cols <- c("neonatal_exist","neonatal_contr","neonatal_sus","neonatal_nsus")

# replace neonatal_beds NA with 0
aop_hospitals[
  ,
  (neonatal_cols) := lapply(.SD, tidyr::replace_na, replace = 0),
  .SDcols = neonatal_cols
]

# 6 save data -------------------------------------------------------
# select columns or save whole data?
readr::write_rds(
  x = aop_hospitals, 
  file = '//STORAGE6/usuarios/Proj_acess_oport/data/acesso_oport/hospitais_equipamentos/hospitais_leitos_neonatal_201812.rds',
  compress = 'gz')


