# Назначение -----------------------------------------------------------
# Скрипт для объединения выгрузок из "Реестра заездов" в АИСО
# в одну таблицу и (опционально) сохранения её в xlsx-файл для
# дальнейшего анализа.
# ВАЖНО! Не нужно переименовывать файлы после разархивирования!
# ВАЖНО! Если все файлы из папки с выгрузками должны быть закрыты!

# Необходимые пакеты ---------------------------------------------------
# install.packages("openxlsx")
# install.packages("readxl")
# library(readxl)
library(openxlsx)

# Указание рабочей директории ------------------------------------------
# Раскомментировать и ввести корректный путь до загруженных 
# и разархивированных файлов.
# setwd("~/aiso/2019/")

# Функции --------------------------------------------------------------

# Объединение выгрузок в одну таблицу
CreateDataset <- function(x)
{
  files <- list.files(path = x, pattern = "Реестр") # создаёт список файлов
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

# Присвоение названий столбцов
colnames(aiso.register) <- c("app_no", "app_no_portal", "voucher_no",
                             "date_time", "status", "denial_reason",
                             "exec_auth", "office", "list", 
                             "payment", "purpose", "vacation_spot",
                             "vacation_address", "period", "theme",
                             "category", "surname_camper", "name_camper",
                             "patronym_camper", "gender_camper", "birthdate_camper",
                             "age_camper", "birthplace_camper", "SNILS_camper", 
                             "ID_camper", "IDser_camper", "IDno_camper",
                             "IDissuedate_camper", "IDissueplace_camper", "disorder_cat",
                             "disorder_sub", "benefit", "reg_address", 
                             "ticket_to_rejection", "ticket_from_rejection", "surname_applicant",
                             "name_applicant", "patronym_applicant", "ID_applicant",
                             "IDser_applicant", "IDno_applicant", "phone",
                             "email", "name_mother", "birthdate_mother",
                             "name_father", "birthdate_father")

# Сохранение файла в формате xlsx --------------------------------------
# write.xlsx(aiso.register, file = "aiso_register.xlsx")
