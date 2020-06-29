# Назначение -----------------------------------------------------------
# Скрипт для объединения выгрузок из "Реестра заездов" в АИСО
# в одну таблицу и (опционально) сохранения её в xlsx-файл для
# дальнейшего анализа.
# ВАЖНО! Не нужно переименовывать файлы после разархивирования!
# ВАЖНО! Все файлы из папки с выгрузками должны быть закрыты!

# Необходимые пакеты ---------------------------------------------------
# install.packages(c("openxlsx", "lubridate", "dplyr"))
library(openxlsx)
library(lubridate)
library(dplyr)

# Указание рабочей директории ------------------------------------------
# Раскомментировать и ввести корректный путь до загруженных 
# и разархивированных файлов.
# setwd("~/aiso/2020/")

# Функции --------------------------------------------------------------

# Объединение выгрузок в одну таблицу
CreateDataset <- function(x)
{
  files <- list.files(path = x, pattern = ".xlsx", recursive = TRUE) # создаёт список файлов
  files.list <- lapply(files, read.xlsx) # создаёт лист необработанных таблиц
  dataset <- lapply(files.list, CreateTable) # обрабатывает таблицы в листе
  y <- do.call(rbind.data.frame, dataset) # объединяет таблицы
  return(y) # возвращает таблицу
}

# Вспомогательная подготовка таблиц к объединению
CreateTable <- function(x)
{
  names(x) <- as.character(unlist(x[1, ])) # забирает заголовки столбцов
  x <- x[-c(1), ] # удаляет строки без данных
}

# Создание массива -----------------------------------------------------
aiso.register <- CreateDataset("./")

# Присвоение названий столбцов -----------------------------------------
colnames(aiso.register) <- c("app_no", "app_no_portal", 
                             "voucher_no", "app_date", 
                             "status", "denial_reason",
                             "exec_auth", "office", 
                             "list", "pay_status", 
                             "purpose", "vacation_spot",
                             "vacation_address", "period", 
                             "theme", "category", "surname_camper", "name_camper",
                             "patronym_camper", "gender_camper", "birthdate_camper",
                             "age_camper", "birthplace_camper", "SNILS_camper", 
                             "ID_camper", "IDser_camper", "IDno_camper",
                             "IDissuedate_camper", "IDissueplace_camper", "disorder_cat",
                             "disorder_sub", "benefit", "reg_address", 
                             "ticket_to_rejection", "ticket_from_rejection", "proxy", "surname_applicant",
                             "name_applicant", "patronym_applicant", "ID_applicant",
                             "IDser_applicant", "IDno_applicant", "phone", 
                             "email", "name_mother", "birthdate_mother",
                             "name_father", "birthdate_father")

aiso.register <- aiso.register %>%
  mutate(app_date = dmy(substring(app_date, 1, 10)),
         birthdate_camper = dmy(birthdate_camper),
         age_camper = year(app_date) - year(birthdate_camper))

source("~/git/ais/aiso/aiso_labels.R", encoding = "UTF-8")

# Сохранение результатов -----------------------------------------------
# (расскомментировать соответствующие строки и задать правильные 
# название для файлов)

# aiso2020 <- aiso.register

# Сохранение массива для анализа в R
# save(aiso2020, file = "~/data/aiso2020_200320.rda")

# Сохранение массива в формате csv для работы в MS Excel / LO Calc
# write.csv2(aiso2020, file = "~/data/aiso2020_200320.csv", row.names = FALSE)
