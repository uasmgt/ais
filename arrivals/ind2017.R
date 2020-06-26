# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции --------------------------------------------------------------
source("~/git/ais/arrivals/arrivals_functions.R", encoding = "UTF-8")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers2017.rda"))
source("~/git/ais/arrivals/beneficiaries.R", encoding = "UTF-8")
# Реестр отказов 
denials.data <- read.csv2("~/data/denials_ind2017.csv", encoding = "UTF-8")
# Данные о лагерях
camps <- read.csv2("~/data/camps.csv", encoding = "UTF-8")

# Создание массива -----------------------------------------------------
setwd("~/aism/2017/arrivals_ind")

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
unique(data.ind) -> ind2017
save(ind2017, file = "~/data/data_ind_2017.rda")
write.csv2(ind2017, file = "~/data/data_ind_2017.csv", row.names = FALSE)

# Очистка окружения
rm(list = setdiff(ls(), "ind2017"))
