# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции --------------------------------------------------------------
source("~/git/ais/arrivals_functions.R")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers_2019.rda"))
# Объединение серии и номера документа отдыхающего в одну переменную
aiso.register$id <- paste(aiso.register$IDser_camper,
                          aiso.register$IDno_camper)
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

# Создание массива
file.names <- list.files(path = "./", recursive = TRUE, 
                          pattern = "*.xlsx") # чтение названий xlsx-файлов
files.list <- lapply(file.names, ReadSheets) # обработка файлов
data.arrivals <- data.frame(matrix(unlist(files.list), 
                                   nrow = length(files.list), byrow = TRUE)) # конвертация листа в таблицу
colnames(data.arrivals) <- c("fact_vouchers", "disabled", "orphans", "needy",
                             "nonarrivals_vouchers", "nonarrivals_by_denial",
                             "fact_dep", "nonarrivals_dep", "fact_total",
                             "mental", "muscle_skeleton", "dysfunction",
                             "sensorial", "disorders_total", 
                             "nonarrivals_total") # присвоение столбцам заголовков
session.data <- lapply(file.names, GetInfo)
session.data <- data.frame(matrix(unlist(session.data), 
                                  nrow = length(session.data), byrow=TRUE))
session.data$session <- lapply(file.names, GetSession)
session.data$session <- as.character(session.data$session)
colnames(session.data) <- c("camp_name", "date_in", "date_out", "session")

staff.data <- lapply(file.names, GetStaff)
staff.data <- data.frame(matrix(unlist(staff.data), nrow = length(staff.data), byrow=TRUE))
colnames(staff.data) <- c("educators")
data.ind <- cbind(session.data, staff.data, data.arrivals)

# Обработка дат --------------------------------------------------------
data.ind$date_in <- as.Date(data.ind$date_in, format = "%d.%m.%Y")
data.ind$date_out <- as.Date(data.ind$date_out, format = "%d.%m.%Y")

# Добавление региона ---------------------------------------------------
data.ind$region <- camps$region[match(data.ind$camp_name, camps$camp_name)]

# Сохранение массива ---------------------------------------------------
unique(data.ind) -> ind2019
save(ind2019, file = "~/data/data_ind_2019.rda")

# Очистка окружения
rm(list = setdiff(ls(), "ind2019"))
