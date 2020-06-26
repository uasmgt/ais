# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)

# Подгрузить функции ---------------------------------------------------
source("~/git/ais/medical/medical_functions.R", encoding = "UTF-8")

# Подгрузить дополнительные данные ------------------------------------------------
data.arrivals <- get(load("~/data/data_ind_2019.rda"))
camps <- read.csv2("~/data/camps.csv", encoding = "UTF-8")

# Названия колонок для заболеваний и травм -----------------------------
dyscrasia <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "tetter")
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "other")

# Сборка массива -------------------------------------------------------
# Рабочая папка 
setwd("~/aism/2019/arrivals_ind/")
# Обработать файлы заездов
file.names <- list.files(path = "./", recursive = TRUE, 
                         pattern = "*.xlsx")
file.list <- lapply(file.names, read.xlsx)
# Создать подмассив со сведениями о заездах
session.data <- lapply(file.list, GetInfo)
session.data <- data.frame(matrix(unlist(session.data), 
                                  nrow=length(session.data), byrow=TRUE))
# Создать подмассив со сведениями об обращениях
medical.data <- lapply(file.list, GetMedical2019)
medical.data <- data.frame(matrix(unlist(medical.data), 
                                  nrow=length(medical.data), byrow=TRUE))
# Конвертировать переменные 
# convert.cols <- c(1:ncol(medical.data))
medical.data[ , c(1:ncol(medical.data))] <- apply(
  medical.data[ , c(1:ncol(medical.data))], 2,
  function(x) as.numeric(as.character(x)))
# Объединить массивы
medical.data <- cbind(session.data, medical.data)
colnames(medical.data) <- c("camp_name", "date_in", "date_out",
                            traumas, "trm_sum", "trm_ins", dyscrasia, 
                            "dys_sum", "dys_ins")

# Обработка дат
medical.data$date_in <- as.Date(medical.data$date_in, 
                                format = "%d.%m.%Y")
medical.data$date_out <- as.Date(medical.data$date_out, 
                                 format = "%d.%m.%Y")

# Задать расположение лагеря
medical.data$region <- camps$region[match(medical.data$camp_name, 
                                          camps$camp_name)]
# Рассчитать продолжительность заезда
medical.data$duration <- medical.data$date_out - medical.data$date_in + 1

# Удаление дубликатов
medical.data <- unique(medical.data)

# Рассчитать доли групп отдыхающих -------------------------------------
source("~/git/ais/medical/medical_percents_ind.R", encoding = "UTF-8")

# Присвоение атрибутов -------------------------------------------------
source("~/git/ais/medical/medical_labels.R", encoding = "UTF-8")

# Сохранение данных
medical.data -> medical.ind2019
save(medical.ind2019, file = "~/data/medical_ind2019.rda")
write.csv2(medical.ind2019, file = "~/data/medical_ind2019.csv",
           row.names = FALSE)

# Очистка окружения ----------------------------------------------------
rm(list = setdiff(ls(), "medical.ind2019"))
