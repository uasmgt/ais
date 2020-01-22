# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)

# Подгрузить функции ---------------------------------------------------
source("~/git/ais/medical/medical_functions.R")

# Подгрузить дополнительные данные ------------------------------------------------
load("~/data/data_ind_2019.rda")
camps <- read.csv2("~/data/camps.csv")

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
session.data <- lapply(file.list, GetIndInfo2019)
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
colnames(medical.data) <- c("camp_name", "session", "date_in", "date_out",
                            traumas, "trm_sum", "trm_ins", dyscrasia, 
                            "dys_sum", "dys_ins")

# Обработка дат
medical.data$date_in <- as.Date(medical.data$date_in, 
                                format = "%d.%m.%Y")
medical.data$date_out <- as.Date(medical.data$date_out, 
                                 format = "%d.%m.%Y")
# Удаление дубликатов
medical.data <- unique(medical.data)

# Задать расположение лагеря
medical.data$region <- camps$region[match(medical.data$camp_name, 
                                          camps$camp_name)]
# Рассчитать продолжительность заезда
medical.data$duration <- medical.data$date_out - medical.data$date_in + 1

# Подгрузить данные о количестве отдыхающих
medical.data$kids <- ind2019$fact_total
medical.data$department <- ind2019$fact_dep
medical.data$disabled <- ind2019$disabled
medical.data$disorders <- ind2019$disorders_total
medical.data$mental <-  ind2019$mental
medical.data$muscle_skeleton <- ind2019$muscle_skeleton
medical.data$dysfunction <- ind2019$dysfunction
medical.data$sensorial <- ind2019$sensorial

# Удалить заезды без отдыхающих ----------------------------------------
medical.data <- medical.data %>% filter(kids != 0)

# Рассчитать доли детей разных категорий от числа детей ----------------
medical.data$per_department <- medical.data$department / medical.data$kids
medical.data$per_disabled <- medical.data$disabled / medical.data$kids
medical.data$per_disorders <- medical.data$disorders / medical.data$kids
medical.data$per_mental <- medical.data$mental / medical.data$kids
medical.data$per_muscle_skeleton <- medical.data$muscle_skeleton /medical.data$kids
medical.data$per_dysfunction <- medical.data$dysfunction / medical.data$kids
medical.data$per_sensorial <- medical.data$sensorial / medical.data$kids

medical.data$dys_per_men <- medical.data$dys_sum / medical.data$kids
medical.data$trm_per_men <- medical.data$trm_sum / medical.data$kids

# Присвоение атрибутов -------------------------------------------------
source("~/git/ais/medical/medical_labels.R", encoding = "UTF-8")

# Сохранение данных
medical.data -> medical.ind2019
save(medical.ind2019, file = "~/data/medical_ind2019.rda")
write.csv2(medical.ind2019, file = "~/data/medical_ind2019.csv",
           row.names = FALSE)

# Очистка окружения ----------------------------------------------------
rm(list = setdiff(ls(), "medical.ind2019"))