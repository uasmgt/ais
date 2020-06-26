# Установка необходимых библиотек (раскомментировать следующую строку)
# install.packages(c("openxlsx", "dplyr", "tidyr"))

# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции --------------------------------------------------------------
source("~/git/ais/arrivals/arrivals_functions.R", encoding = "UTF-8")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers2018.rda"))
source("~/git/ais/arrivals/beneficiaries.R", encoding = "UTF-8")

# Данные о лагерях
camps <- read.csv2("~/data/camps.csv", encoding = "UTF-8")

# Создание массива -----------------------------------------------------
setwd("~/aism/2018/arrivals_fam")

file.names <- list.files(path = "./", recursive = TRUE, 
                         pattern = "*.xlsx") # чтение названий xlsx-файлов
data.arrivals <- lapply(file.names, CountFam)
data.arrivals <- data.frame(matrix(unlist(data.arrivals), 
                                   nrow = length(data.arrivals), 
                                   byrow = TRUE))
colnames(data.arrivals) <- c("family_visits", "kids_visits", "disabled", 
                             "orphans", "needy", "parents_visits", "family_non",
                             "kids_non", "parents_non", "youth_visits",
                             "youth_non", "add_visits", "add_kids_visits",
                             "add_parents_visits", "add_non", "add_kids_non",
                             "add_parents_non", "dep_visits", "dep_orphans_u7_visits",
                             "dep_orphans_o18_visits", "dep_educators_visits",
                             "dep_non", "dep_orphans_u7_non", "dep_orphans_o18_non",
                             "dep_educators_non", "visits_total", 
                             "mental", "muscle_skeleton", "dysfunction",
                             "sensorial", "disorders_total", "non_total")
session.data <- lapply(file.names, GetInfo)
session.data <- data.frame(matrix(unlist(session.data), 
                                  nrow = length(session.data), byrow = TRUE))
colnames(session.data) <- c("camp_name", "date_in", "date_out")
data.fam <- cbind(session.data, data.arrivals)

# Обработка дат --------------------------------------------------------
data.fam$date_in <- as.Date(data.fam$date_in, format = "%d.%m.%Y")
data.fam$date_out <- as.Date(data.fam$date_out, format = "%d.%m.%Y")

# Добавление информации о расположении лагерей -------------------------
data.fam$region <- camps$region[match(data.fam$camp_name, camps$camp_name)]

# Сохранение массива ---------------------------------------------------
unique(data.fam) -> fam2018
save(fam2018, file = "~/data/data_fam_2018.rda")
write.csv2(fam2018, file = "~/data/data_fam_2018.csv", row.names = FALSE)

# Очистка окружения
rm(list = setdiff(ls(), "fam2018"))
