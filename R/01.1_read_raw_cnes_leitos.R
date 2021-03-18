source('./R/0_setup.R')

# list all files
lt_files <- list.files('\\\\storage1\\ContasSHA\\Contas\\CNES\\LT', full.names = T)

# subset December 2018
lt_files <- lt_files[ lt_files %like% '1812']

# read all files
df <- pblapply(X=lt_files, FUN=read.dbc)
df <- rbindlist(df)

# save raw data
fwrite(df, './../../data-raw/hospitais_equipamentos/cnes_let_201812.csv')
