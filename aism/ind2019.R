# Необходимые пакеты ---------------------------------------------------
# install.packages(c("openxlsx", "dplyr", "tydir"))
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции
source("~/git/ais/aism/functions.R", encoding = "UTF-8")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers2019.rda"))
source("~/git/ais/aism/beneficiaries.R", encoding = "UTF-8")
# Реестр отказов 
denials.data <- read.csv2("~/data/denials_ind2019.csv", encoding = "UTF-8")
# Данные о лагерях
camps <- read.xlsx("~/data/camps.xlsx")

# Названия колонок для заболеваний и травм -----------------------------
dyscrasia <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "tetter")
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "other")

# Создание массивов ----------------------------------------------------
setwd("~/aism/2019/arrivals_ind")

file.names <- list.files(path = "./", recursive = TRUE, 
                         pattern = "*.xlsx") # чтение названий xlsx-файлов
arrivals.list <- lapply(file.names, ReadSheets) # обработка файлов
data.arrivals <- data.frame(matrix(unlist(arrivals.list), 
                                   nrow = length(arrivals.list), byrow = TRUE)) # конвертация листа в таблицу
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
# Добавление региона 
session.data$region <- camps$region[match(session.data$camp_name, camps$camp_name)]
# Короткое название лагеря
session.data$camp_name <- camps$short_name[match(session.data$camp_name, camps$camp_name)]
# Обработка дат 
session.data$date_in <- as.Date(session.data$date_in, format = "%d.%m.%Y")
session.data$date_out <- as.Date(session.data$date_out, format = "%d.%m.%Y")
# Продолжительность заезда
session.data$duration <- as.numeric(session.data$date_out - session.data$date_in + 1)

staff.data <- lapply(file.names, GetStaff)
staff.data <- data.frame(matrix(unlist(staff.data), nrow = length(staff.data), byrow=TRUE))
colnames(staff.data) <- c("educators")

medical.data <- lapply(file.names, GetMedicalCur)
medical.data <- do.call(rbind.data.frame, medical.data)
medical.data[ , c(1:ncol(medical.data))] <- apply(
  medical.data[ , c(1:ncol(medical.data))], 2,
  function(x) as.numeric(as.character(x)))
colnames(medical.data) <- c(traumas, "trm_sum", "trm_ins", dyscrasia, 
                            "dys_sum", "dys_ins")
# Сборка массива ----
dataset <- cbind(session.data, staff.data, data.arrivals, medical.data)
dataset <- unique(dataset)
rm(list = setdiff(ls(), "dataset")) # очистка окружения

# дополнительные расчёты
source("~/git/ais/aism/calc_ind.R", encoding = "UTF-8")
# пояснение к названиям колонок
source("~/git/ais/aism/labels_ind.R", encoding = "UTF-8")

# Сохранение данных ----------------------------------------------------
ind2019 <- dataset[c(1:22, 45, 46)]
med.ind2019 <- dataset[c(1:6, 13, 16, 23:44)]

# для анализа в R
save(ind2019, file = "~/data/arrivals_ind2019.rda")
save(med.ind2019, file = "~/data/medical_ind2019.rda")

# для анализа в Excel
write.csv2(ind2019, file = "~/data/arrivals_ind2019.csv")
write.csv2(med.ind2019, file = "~/data/medical_ind2019.csv")
