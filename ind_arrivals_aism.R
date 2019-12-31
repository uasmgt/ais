# Назначение -----------------------------------------------------------
# Агрегация данных о заездах для индивидуального отдыха в ходе ЛОК-2019:
# сведений о недозаездах получателей путёвок и количестве отдыхающих
# разных льготных категорий

# Предварительные процедуры --------------------------------------------
# Выгрузки по анализируемым заездам должны быть помещены в папку 
# "~/plan_fact/arrivals_ind" для корректной обработки данных,  
# или же нужно изменить путь до файлов с выгрузками в соответствующей
# строке.

# Также необходимо указать путь до агрегированного реестра заявлений из
# АИС "Отдых" в формате xlsx
# (см.: https://github.com/uasmgt/ais/blob/master/register_aiso.R).

# Пакеты ---------------------------------------------------------------
# install.packages(c("openxlsx", "dplyr", "tidyr"))
library(openxlsx)
library(dplyr)
library(tidyr)

# Рабочая директория
setwd("~/plan_fact/")

# Дополнительные данные (реестр заявлений) ----
aiso.register <- read.xlsx("register_data.xlsx")
aiso.register <- aiso.register[c(1, 11, 26, 27, 32)]
aiso.register$id <- paste(aiso.register$Серия, 
                          aiso.register$Номер)

# Дополнительные данные (реестр отказов) ----
denials.data <- read.xlsx("denials.xlsx")

# Создание подмассивов с заявителями различных льготных категорий
register.disabled <- aiso.register %>% filter(`Вид.льготы` == "Дети-инвалиды, дети с ограниченными возможностями здоровья")
register.orphans <- aiso.register %>% filter(`Вид.льготы` == "Дети-сироты и дети, оставшиеся без попечения родителей, находящиеся под опекой, попечительством, в том числе в приемной или патронатной семье")
register.needy <- aiso.register %>% filter(`Вид.льготы` == "Дети из малообеспеченных семей")

# Функции -----
setwd("arrivals_ind/")

# сбор сведений о заезде
SheetsSession <- function(x){
  file <- read.xlsx(x, sheet = 1)
}

GetSessionInfo <- function(x){
  if (ncol(x) == 23 | ncol(x) == 24){             # проверяет количество столбцов
    camp <- x[1, 2]                               # название лагеря
    sess <- x[1, 14]                              # номер смены
    term <- as.character(x[1, 21])                # период заезда
    term <- unlist(strsplit(term, split = " - ")) # разбивка периода заезда на дату заезда и выезда
    date.in  <- term[1]                           # дата заезда
    date.out <- term[2]                           # дата выезда
    info <- cbind(camp, sess, date.in, date.out)  # запись переменных в строку
  } else if (ncol(x) == 14){
    camp <- x[1, 2]
    sess <- x[1, 6]
    term <- as.character(x[1, 8])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    info <- cbind(camp, sess, date.in, date.out)
  } else if (ncol(x) == 18){
    camp <- x[1, 2]
    sess <- x[1, 10]
    term <- as.character(x[1, 14])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    info <- cbind(camp, sess, date.in, date.out)
  } else if (ncol(x) == 9){
    camp <- x[1, 2]
    sess <- x[1, 6]
    term <- as.character(x[1, 14])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    info <- cbind(camp, sess, date.in, date.out)
  } else if (ncol(x) == 12){
    camp <- x[1, 2]
    sess <- x[1, 6]
    term <- as.character(x[1, 8])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    info <- cbind(camp, sess, date.in, date.out)
  } else {
    info <- cbind(NA, NA, NA, NA)
    }
  return(info)
}

# сбор сведений о количестве воспитателей
SheetsStaff <- function(x){
  file <- read.xlsx(x, sheet = 2)
}

GetStaffInfo <- function(x){
  if (ncol(x) == 13) { #  проверяет кол-во столбцов (заполненность листа)
    # подготовка xlsx-файла к анализу
    names(x) <- as.character(unlist(x[3, ])) # забирает названия столбцов
    x <- x[-c(1:3), ]                        # удаляет лишние строки
    tutors.num <- nrow(x %>% filter(`Должность` == "Воспитатель")) # считает количество воспитателей
    tutors.num <- cbind(tutors.num)} else {
      tutors.num = 0                                               # записывает ноль, если лист не заполнен
    }
  return(tutors.num)
}

# сбор сведений о детях
CreateRow <- function(x){
  # готовит xlsx-файл для анализа
  names(x) <- as.character(unlist(x[4, ])) # забирает названия столбцов
  x <- x[-c(1:4), ]                                   # удаляет мусорные строки
  x <- x[c(1, 2, 5, 7, 8, 10, 11, 12, 13, 27)]               # оставляет нужные для работы столбцы
  x <- x %>% filter(`Ребёнок/вожатый` == "Ребёнок")   # фильтрует детей
  # создание подмассивов
  arrivals <- x %>% filter(`Заехал` == "Заехал")         # фильтрует заехавших
  non.arrivals <- x %>% filter(`Заехал` == "Не заехал")   # фильтрует незаехавших
  departament <- x %>% filter(`Организация списка` != "") # фильтрует детей от ДТСЗН
  portal <- subset(x, grepl("[0-9]{4}\\-[0-9]{7}\\-[0-9]{6}\\-[0-9]{7}\\/[0-9]{2}", `Номер заявления`)) # фильтрует получателей путёвок
  # Заехали
  arrivals.sum <- nrow(arrivals) # всего заехало
  arrivals.dep <- nrow(departament %>% filter(`Заехал` == "Заехал")) # заехало от ДТСЗН
  arrivals.por <- portal %>% filter(`Заехал` == "Заехал") # заехало по путёвкам
  arrivals.por.num <- nrow(arrivals.por) # количество заехавших по путёвкам
  # не заехали
  non.arrivals.sum <- nrow(non.arrivals) # всего не заехало
  non.arrivals.dep <- nrow(departament %>% filter(`Заехал` == "Не заехал")) # не заехало от ДТСЗН
  non.arrivals.por <- portal %>% filter(`Заехал` == "Не заехал") # не заехало по путёвке
  non.arrivals.por.num <- nrow(non.arrivals.por)
  non.arrivals.by.denial <- nrow(non.arrivals.por[non.arrivals.por$`ФИО` %in% denials.data$name, ]) # считает недозаезды из-за медотводов
  # подсчёт отдыхающих по категориям
  disabled <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% register.disabled$id, ]) # инвалиды
  needy <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% register.needy$id, ])       # малообеспеченные
  orphans <- nrow(arrivals.por[arrivals.por$`Номер документа` %in% register.orphans$id, ])   # сироты
  
  row <- cbind(arrivals.por.num, disabled, orphans, needy, 
               non.arrivals.por.num, non.arrivals.by.denial,
               arrivals.dep, non.arrivals.dep,
               arrivals.sum, non.arrivals.sum)  # записывает данные в строку
}

# Обработка xlsx-файлов
ReadSheets <- function(x){
  n.sheets <- getSheetNames(x)                      # забирает названия листов в xlsx-файле
  l.sheets <- as.list(rep(NA, c(length(n.sheets)))) # создаёт лист с листами (ха-ха)
  names(l.sheets) <- n.sheets                       # считает листы
  for (i in 1:length(n.sheets)) {                   # загружает все xlsx в папке
    l.sheets[[i]] <- read.xlsx(x, sheet = i)
  }
  l.sheets <- l.sheets[-c(1, 2)]                    # убирает листы с информацией о заезде и персонале
  l.sheets <- Filter(function(x) {ncol(x) == 30}, l.sheets) # фильтрует листы с отрядами
  data <- lapply(l.sheets, CreateRow)               # создаёт строки по данным отрядов заезда
  data <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE)) # создаёт табличное представление строки
  string <- data.frame(colSums(data)) # складывает данные по отрядам
  return(string)
}

# Создание массива
# Получение сведений о заехавших
files.ind <- list.files(path = "./", recursive = TRUE, pattern = "*.xlsx")
list.ind <- lapply(files.ind, ReadSheets)
dataset.ind <- data.frame(matrix(unlist(list.ind), nrow = length(list.ind), byrow = TRUE))
colnames(dataset.ind) <- c("Факт (путёвки)", "Инвалиды", "Сироты",
                           "Малооб.", "Недозаезды (путёвки)", "Н/з медотвод (путёвки)",
                           "Факт (департамент)", "Недозаезды (департамент)", 
                           "Факт (всего)", "Недозаезды (всего)")

list.info <- lapply(files.ind, SheetsSession)
info.ind <- lapply(list.info, GetSessionInfo)
dataset.info <- data.frame(matrix(unlist(info.ind), nrow = length(info.ind), byrow=TRUE))
colnames(dataset.info) <- c("Название лагеря", "Смена", "Дата заезда", "Дата выезда")

list.staff <- lapply(files.ind, SheetsStaff)
indo.staff <- lapply(list.staff, GetStaffInfo)
dataset.staff <- data.frame(matrix(unlist(indo.staff), nrow = length(indo.staff), byrow=TRUE))
colnames(dataset.staff) <- c("Кол-во воспитателей")

data.ind <- cbind(dataset.info, dataset.staff, dataset.ind)

data.ind$`Дата заезда` <- as.Date(data.ind$`Дата заезда`, format = "%d.%m.%Y")
data.ind$`Дата выезда` <- as.Date(data.ind$`Дата выезда`, format = "%d.%m.%Y")
data.ind$`Продолжительность смены` <- as.numeric(data.ind$`Дата выезда` - data.ind$`Дата заезда` + 1)

data.ind <- data.ind[c(1:4, 16, 5:15)] # КАК-НИБУДЬ ПРОВЕРИТЬ, МОЖЕТ СЛОМАТЬСЯ

# geography ----
setwd("..")
camps <- read.xlsx("geography2.xlsx")
data.ind$`Зона` <- camps$zone[match(data.ind$`Название лагеря`, camps$name)]
data.ind$`Регион` <- camps$region[match(data.ind$`Название лагеря`, camps$name)]
data.ind$`Адрес` <- camps$address[match(data.ind$`Название лагеря`, camps$name)]
data.ind <- data.ind[c(1, 16:18, 2:15)]
data.ind <- data.ind %>% drop_na(`Название лагеря`)

# write data ----
write.xlsx(data.ind, "xlsxs/data_individual2019.xlsx")
