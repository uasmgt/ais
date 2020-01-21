# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Directory
setwd("~/aism/2018")

# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers_2018.rda"))
rm(vouchers.aiso.2018)

aiso.register$id <- paste(register.data$IDser_camper, 
                          register.data$IDno_camper)
aiso.register <- aiso.register %>% filter(benefit == "Дети-инвалиды, дети с ограниченными возможностями здоровья" |
                                            benefit == "Дети-инвалиды")
aiso.register <- aiso.register %>% filter(benefit == "Дети-сироты и дети, оставшиеся без попечения родителей, находящиеся под опекой, попечительством, в том числе в приемной или патронатной семье" |
                                            benefit == "Дети-сироты")
aiso.register <- aiso.register %>% filter(benefit == "Дети из малообеспеченных семей")
aiso.register <- aiso.register %>% filter(purpose == "Молодёжный отдых для лиц из числа детей-сирот и детей, оставшихся без попечения родителей, 18-23 лет")
