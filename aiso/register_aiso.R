# Назначение -----------------------------------------------------------
# Скрипт для объединения выгрузок из "Реестра заездов" в АИСО
# в одну таблицу и (опционально) сохранения её в xlsx-файл для
# дальнейшего анализа.
# ВАЖНО! Не нужно переименовывать файлы после разархивирования!
# ВАЖНО! Если все файлы из папки с выгрузками должны быть закрыты!

# Необходимые пакеты ---------------------------------------------------
# install.packages("openxlsx")
library(openxlsx)
library(lubridate)

# Указание рабочей директории ------------------------------------------
# Раскомментировать и ввести корректный путь до загруженных 
# и разархивированных файлов.
setwd("~/aiso/2014/vouchers/")

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

# Присвоение названий столбцов -----------------------------------------
colnames(aiso.register) <- c("app_no", "app_no_portal", 
                             "voucher_no", "date_time", 
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

aiso.register <- data.frame(aiso.register)
aiso.register$date_time <- as.POSIXct(aiso.register$date_time, format="%d.%m.%Y")
aiso.register$birthdate_camper <- as.POSIXct(aiso.register$birthdate_camper, format="%d.%m.%Y")
aiso.register$age_camper <-  year(aiso.register$date_time) - year(aiso.register$birthdate_camper)

# Присвоение столбцам атрибутов ----------------------------------------
attr(aiso.register$app_no, "label") <- "Номер заявления"
attr(aiso.register$app_no_portal, "label") <- "Номер заявления с МПГУ"
attr(aiso.register$voucher_no, "label") <- "Номер путёвки"
attr(aiso.register$date_time, "label") <- "Дата заявления"
attr(aiso.register$status, "label") <- "Статус заявления"
attr(aiso.register$denial_reason, "label") <- "Причина отказа"
attr(aiso.register$exec_auth, "label") <- "ОИВ"
attr(aiso.register$office, "label") <- "Учреждение"
attr(aiso.register$list, "label") <- "Список"
attr(aiso.register$pay_status, "label") <- "Статус оплаты"
attr(aiso.register$purpose, "label") <- "Цель обращения"
attr(aiso.register$vacation_spot, "label") <- "Место отдыха"
attr(aiso.register$vacation_address, "label") <- "Адрес оздоровительной организации"
attr(aiso.register$period, "label") <- "Время отдыха"
attr(aiso.register$theme, "label") <- "Тематика смены"
attr(aiso.register$category, "label") <- "Категория отдыхающего"
attr(aiso.register$surname_camper, "label") <- "Отдыхающий: Фамилия"
attr(aiso.register$name_camper, "label") <- "Отдыхающий: Имя"
attr(aiso.register$patronym_camper, "label") <- "Отдыхающий: Отчество"
attr(aiso.register$gender_camper, "label") <- "Отдыхающий: пол"
attr(aiso.register$birthdate_camper, "label") <- "Отдыхающий: дата рождения"
attr(aiso.register$age_camper, "label") <- "Отдыхающий: возраст"
attr(aiso.register$birthplace_camper, "label") <- "Отдыхающий: место рождения"
attr(aiso.register$SNILS_camper, "label") <- "Отдыхающий: СНИЛС"
attr(aiso.register$ID_camper, "label") <- "Отдыхающий: документ"
attr(aiso.register$IDser_camper, "label") <- "Отдыхающий: серия документа"
attr(aiso.register$IDno_camper, "label") <- "Отдыхающий: номер документа"
attr(aiso.register$IDissuedate_camper, "label") <- "Отдыхающий: дата выдачи документа"
attr(aiso.register$IDissueplace_camper, "label") <- "Отдыхающий: кем выдан документ"
attr(aiso.register$disorder_cat, "label") <- "Отдыхающий: вид ограничения"
attr(aiso.register$disorder_sub, "label") <- "Отдыхающий: подвид ограничения"
attr(aiso.register$benefit, "label") <- "Отдыхающий: вид льготы"
attr(aiso.register$reg_address, "label") <- "Отдыхающий: адрес регистрации"
attr(aiso.register$ticket_to_rejection, "label") <- "Отдыхающий: отказ от билета (в место отдыха)"
attr(aiso.register$ticket_from_rejection, "label") <- "Отдыхающий: отказ от билета (из места отдыха)"
attr(aiso.register$proxy, "label") <- "Доверенность"
attr(aiso.register$surname_applicant, "label") <- "Заявитель: Фамилия"
attr(aiso.register$name_applicant, "label") <- "Заявитель: Имя"
attr(aiso.register$patronym_applicant, "label") <- "Заявитель: Отчество"
attr(aiso.register$ID_applicant, "label") <- "Заявитель: документ"
attr(aiso.register$IDser_applicant, "label") <- "Заявитель: серия документа"
attr(aiso.register$IDno_applicant, "label") <- "Заявитель: номер документа"
attr(aiso.register$phone, "label") <- "Заявитель: телефон"
attr(aiso.register$email, "label") <- "Заявитель: эл. почта"
attr(aiso.register$name_mother, "label") <- "Мать: ФИО"
attr(aiso.register$birthdate_mother, "label") <- "Мать: дата рождения"
attr(aiso.register$name_father, "label") <- "Отец: ФИО"
attr(aiso.register$birthdate_father, "label") <- "Отец: дата рождения"

# Сохранение результатов -----------------------------------------------
# (расскомментировать соответствующие строки)
# Сохранение массива для анализа в R
# save(aiso.register, file = "aiso_register.rda")

# Сохранение массива в формате csv для работы в MS Excel / LO Calc
# write.csv2(aiso.register, file = "aiso_register.csv", row.names = FALSE)
