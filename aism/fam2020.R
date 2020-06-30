# Необходимые пакеты ---------------------------------------------------
# install.packages(c("openxlsx", "dplyr", "tydir"))
library(openxlsx)
library(dplyr)
library(tidyr)

# Функции
source("~/git/ais/aism/functions.R", encoding = "UTF-8")

# Дополнительные данные ------------------------------------------------
# Данные из АИСО
aiso.register <- get(load("~/data/aiso_vouchers2020.rda"))
source("~/git/ais/aism/beneficiaries.R", encoding = "UTF-8")
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
setwd("~/aism/2020/arrivals_fam")

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

medical.data <- lapply(file.names, GetMedicalCur)
medical.data <- do.call(rbind.data.frame, medical.data)
medical.data[ , c(1:ncol(medical.data))] <- apply(
  medical.data[ , c(1:ncol(medical.data))], 2,
  function(x) as.numeric(as.character(x)))
colnames(medical.data) <- c(traumas, "trm_sum", "trm_ins", dyscrasia, 
                            "dys_sum", "dys_ins")
# Сборка массива ----
dataset <- cbind(session.data, data.arrivals, medical.data)
dataset <- unique(dataset)
rm(list = setdiff(ls(), "dataset")) # очистка окружения
dataset <- dataset %>% filter(visits_total + non_total != 0)
dataset$dys_per_men <- dataset$dys_sum / dataset$visits_total
dataset$trm_per_men <- dataset$trm_sum / dataset$visits_total
# пояснение к названиям колонок
source("~/git/ais/aism/labels_fam.R", encoding = "UTF-8")

# Сохранение данных ----------------------------------------------------
# fam2020 <- dataset[c(1:3, 5:38)]
# med.fam2020 <- dataset[c(1:3, 5:6, 32, 39:62)]
data.fam2020 <- dataset

# для анализа в R
save(data.fam2020, file = "~/data/data_fam2020.rda")
# для анализа в Excel
write.csv2(data.fam2020, file = "~/data/data_fam2020.csv")
