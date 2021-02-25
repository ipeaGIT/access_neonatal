
library(read.dbc) 
library(tidyverse)
library(data.table)
library(readxl)
library(foreign)
library(pbapply)


options(scipen=10000)

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)
