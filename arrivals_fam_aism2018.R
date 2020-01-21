# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Directory
setwd("~/aism/2018")

# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers_2018.rda"))
rm(vouchers.aiso.2018)
