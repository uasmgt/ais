# Назначение -----------------------------------------------------------
# Обработка данных о заездах для индивидуального отдыха из АИС "МОСГОРТУР".
# Подсчитывает количество заездов / недозаездов в лагерь, количество
# отдыхающих разных льготных категорий и добавляет информацию о
# расположении лагеря.
# ВАЖНО! Данные должны быть сохранены в папку под названием "arrivals_ind".
# Дополнительные данные сохранить в корневую папку.
# Если скрипт не выполняется, то наиболее вероятные причины "поломки":
# 1) в выгрузку попал заезд для совместного отдыха (можно отфильтровать
# поиска в файловом менеджере [точно работает в MS Windows и MacOS]);
# 2) в выгрузку попал заезд для индивидуального отдыха с пустым
# списком отдыхающих (можно найти, упорядочив по размеру файла: как 
# правило таким заездам соответствуют наименьшие по размеру файлы).

# Необходимые пакеты ---------------------------------------------------
library(openxlsx)
library(dplyr)
library(tidyr)

# Указание рабочей директории ------------------------------------------
# Раскомментировать и ввести корректный путь до загруженных 
# и разархивированных файлов.
setwd("~/aism/2019/arrivals_ind/")

# Дополнительные данные (реестр заявлений) -----------------------------
# В случае необходимости обработать выгрузку из АИС "Отдых" скриптом
# register_aiso.R (https://github.com/uasmgt/ais/blob/master/register_aiso.R).
aiso.register <- get(load("~/data/aiso_vouchers_2019.rda"))
rm(vouchers.aiso.2019)
# Объединение серии и номера документа отдыхающего в одну переменную
aiso.register$id <- paste(aiso.register$IDser_camper,
                          aiso.register$IDno_camper)
# Создание подмассивов отдыхающих различных по наиболее крупным льготным
# категориям
aiso.disabled <- aiso.register %>% 
  filter(benefit == 
           "Дети-инвалиды, дети с ограниченными возможностями здоровья")
aiso.orphans <- aiso.register %>% 
  filter(benefit ==
           "Дети-сироты и дети, оставшиеся без попечения родителей, находящиеся под опекой, попечительством, в том числе в приемной или патронатной семье")
aiso.needy <- aiso.register %>% 
  filter(benefit ==
           "Дети из малообеспеченных семей")

# Дополнительные данные (реестр отказов) -------------------------------
# Реестр отказов формируется Управлением по координации организаций
# отдыха и оздоровления по итогам заездов в смену, для анализа 
# достаточно сформировать таблицу (рекомендуется формат csv) с одним 
# столбцом под названием "name" с фамилией, именем, отчеством ребёнка.
denials.data <- read.csv2("~/data/denials_ind2019.csv", encoding = "UTF-8")

# Дополнительные данные (расположение и адрес лагерей) -----------------
load("~/aism/camps.rda")

# Функции --------------------------------------------------------------
# Обработка сведений о заезде ------------------------------------------
# Подгрузка данных с первых листов xlsx файлов в выгрузке
SheetsSession <- function(x){
  file <- read.xlsx(x, sheet = 1)
}
# Чтение данных о заезде из файла
GetSessionInfo <- function(x){
  camp <- x[1, 2] # название лагеря
  if (ncol(x) == 23 | ncol(x) == 24){
    sess <- x[1, 14]
    term <- as.character(x[1, 21])
  } else if (ncol(x) >= 16){
    sess <- x[1, 10]
    term <- as.character(x[1, 14])
  } else if (ncol(x) <= 14){
    sess <- x[1, 6]
    term <- as.character(x[1, 8])
  } else {
    sess <- NA
    term <- " - "
  }
  term <- unlist(strsplit(term, split = " - ")) # разбивка периода отдыха на даты заезда и выезда
  date.in  <- term[1]                           # дата заезда
  date.out <- term[2]                           # дата выезда
  info <- cbind(camp, sess, date.in, date.out)
  return(info) # возврат массива
}

# Обработка данных о количестве пед. персонала -------------------------
# Подгрузка данных со вторых листов xlsx файлов в выгрузке
SheetsStaff <- function(x){
  file <- read.xlsx(x, sheet = 2)
}
# Чтение данных о количестве воспитателей
GetStaffInfo <- function(x){
  if (ncol(x) == 13) { #  проверка заполнения данных
    names(x) <- as.character(unlist(x[3, ])) # чтение заголовков столбцов
    x <- x[-c(1:3), ] # удаление пустых строк
    tutors.num <- nrow(x %>% filter(`Должность` == "Воспитатель")) # подсчёт воспитателей
    tutors.num <- cbind(tutors.num)} else { # запись нолей для незаполненных листов
      tutors.num = 0
    }
  return(tutors.num) # возврат массива
}

# Обработка данных об отдохнувших --------------------------------------
# Обработка xlsx-файла и генерация строки с данными о заезде
CreateRow <- function(x){
  # Подготовка xlsx-файла к анализу
  names(x) <- as.character(unlist(x[4, ]))     # чтение заголовков столбцов
  x <- x[-c(1:4), ]                            # удаление пустых строк
  x <- x[c(1, 2, 5, 7, 8, 10, 11, 12, 13, 27)] # удаление лишних столбцов
  x <- x %>% filter(`Ребёнок/вожатый` == "Ребёнок") # отбор строк со сведениями о детях
  # Создание подмассивов
  arrivals <- x %>% filter(`Заехал` == "Заехал") # отбор заехавших
  non.arrivals <- x %>% filter(`Заехал` == "Не заехал") # отбор незаехавших
  departament <- x %>% filter(`Организация списка` != "") # отбор детей от ДТСЗН
  portal <- subset(x, 
                   grepl("[0-9]{4}\\-[0-9]{7}\\-[0-9]{6}\\-[0-9]{7}\\/[0-9]{2}", 
                         `Номер заявления`)) # отбор получателей путёвок
  # Заезды
  arrivals.sum <- nrow(arrivals) # заезды (всего)
  arrivals.dep <- nrow(departament %>% filter(`Заехал` == "Заехал")) # заезды (ДТСЗН)
  arrivals.por <- portal %>% filter(`Заехал` == "Заехал") # заезды (по путёвкам)
  arrivals.por.num <- nrow(arrivals.por) # подсчёт заездов по путёвкам
  # Недозаезды
  non.arrivals.sum <- nrow(non.arrivals) # недозаезды (всего)
  non.arrivals.dep <- nrow(departament %>% filter(`Заехал` == "Не заехал")) # недозаезды (ДТСЗН)
  non.arrivals.por <- portal %>% filter(`Заехал` == "Не заехал") # недозаезды по путёвкам
  non.arrivals.por.num <- nrow(non.arrivals.por) # подсчёт недозаездов по путёвкам
  non.arrivals.by.denial <- nrow(non.arrivals.por[non.arrivals.por$`ФИО` %in% denials.data$name, ]) # подсчёт недозаездов из-за отказов в допуске
  # Подсчёт заехавших по категориям
  disabled <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% aiso.disabled$id, ]) # инвалиды
  needy <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% aiso.needy$id, ])       # малообеспеченные
  orphans <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% aiso.orphans$id, ])   # сироты
  # Запись подсчитанных данных в строку
  row <- cbind(arrivals.por.num, disabled, orphans, needy, 
               non.arrivals.por.num, non.arrivals.by.denial,
               arrivals.dep, non.arrivals.dep,
               arrivals.sum, non.arrivals.sum)
}
# Обработка всех xlsx-файлов в папке
ReadSheets <- function(x){
  n.sheets <- getSheetNames(x) # сбор заголовков листов в xlsx-файле
  l.sheets <- as.list(rep(NA, c(length(n.sheets)))) # создание листа с листами (ха-ха)
  names(l.sheets) <- n.sheets                       # чтение листов
  for (i in 1:length(n.sheets)) {                   # обработка xlsx-файлов
    l.sheets[[i]] <- read.xlsx(x, sheet = i)
  }
  l.sheets <- l.sheets[-c(1, 2)]                    # удаление листов со сведениями о заезде и пед. персонале
  l.sheets <- Filter(function(x) {ncol(x) == 30}, l.sheets) # обработка листов с отрядами
  data <- lapply(l.sheets, CreateRow)               # создание по данным отрядов заезда
  data <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE)) # табличное представление строки
  string <- data.frame(colSums(data)) # суммирование данных по отрядам
  return(string)
}

# Создание массива
files.ind <- list.files(path = "./", 
                        recursive = TRUE, 
                        pattern = "*.xlsx") # чтение названий xlsx-файлов
list.ind <- lapply(files.ind, ReadSheets) # обработка файлов
dataset.ind <- data.frame(matrix(unlist(list.ind), 
                                 nrow = length(list.ind), byrow = TRUE)) # конвертация листа в таблицу
colnames(dataset.ind) <- c("fact_vouchers", "disabled", "orphans",
                           "needy", "nonarrivals_vouchers", "nonarrivals_by_denial",
                           "fact_dep", "nonarrivals_dep", "fact_total",
                           "nonarrivals_total") # присвоение столбцам заголовков
list.info <- lapply(files.ind, SheetsSession)
info.ind <- lapply(list.info, GetSessionInfo)
dataset.info <- data.frame(matrix(unlist(info.ind), nrow = length(info.ind), byrow=TRUE))
colnames(dataset.info) <- c("camp_name", "session", "date_in", "date_out")
list.staff <- lapply(files.ind, SheetsStaff)
info.staff <- lapply(list.staff, GetStaffInfo)
dataset.staff <- data.frame(matrix(unlist(info.staff), nrow = length(info.staff), byrow=TRUE))
colnames(dataset.staff) <- c("educators")
data.ind <- cbind(dataset.info, dataset.staff, dataset.ind)

# Удаление вспомогательных массивов
rm(list = ls(pattern = "(dataset.)|(info.)|(list.)|(files.)")) 

# Обработка дат --------------------------------------------------------
data.ind$date_in <- as.Date(data.ind$date_in, format = "%d.%m.%Y")
data.ind$date_out <- as.Date(data.ind$date_out, format = "%d.%m.%Y")

# Добавление информации о расположении лагерей -------------------------
setwd("..")
# data.ind$zone <- camps$zone[match(data.ind$camp_name, camps$camp_name)]
data.ind$region <- camps$region[match(data.ind$camp_name, camps$camp_name)]
# data.ind$address <- camps$address[match(data.ind$camp_name, camps$camp_name)]
# data.ind <- data.ind %>% drop_na(camp_name)
data.ind <- unique(data.ind)

# Сохранение результатов -----------------------------------------------
# (расскомментировать соответствующие строки)
# Сохранение массива для анализа в R
# save(data.ind, file = "data_ind.rda")

# Сохранение массива в формате csv для работы в MS Excel / LO Calc
# write.csv2(data.ind, file = "data_ind.csv", row.names = FALSE)
