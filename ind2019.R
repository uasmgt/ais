# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции --------------------------------------------------------------
source("~/git/ais/medical/medical_functions.R")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers_2019.rda"))
aiso.disabled <- aiso.register %>% 
  filter(benefit == 
           "Дети-инвалиды, дети с ограниченными возможностями здоровья")
aiso.orphans <- aiso.register %>% 
  filter(benefit ==
           "Дети-сироты и дети, оставшиеся без попечения родителей, находящиеся под опекой, попечительством, в том числе в приемной или патронатной семье")
aiso.needy <- aiso.register %>% 
  filter(benefit ==
           "Дети из малообеспеченных семей")
# Реестр отказов 
denials.data <- read.csv2("~/data/denials_ind2019.csv", 
	encoding = "UTF-8")
# Рабочая директория
setwd("~/aism/2019/arrivals_ind")
# Данные о лагерях
camps <- read.csv2("~/data/camps.csv")
