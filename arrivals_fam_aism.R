# Назначение -----------------------------------------------------------
# Обработка данных о заездах для совместного отдыха из АИС "МОСГОРТУР".
# Подсчитывает количество заездов / недозаездов в лагерь, количество
# отдыхающих разных льготных категорий и добавляет информацию о
# расположении лагеря.
# ВАЖНО! Данные должны быть сохранены в папку под названием "arrivals_fam".
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

# Directory
setwd("~/aism/2019")

# Дополнительные данные (реестр заявлений) -----------------------------
# В случае необходимости обработать выгрузку из АИС "Отдых" скриптом
# register_aiso.R (https://github.com/uasmgt/ais/blob/master/register_aiso.R).
load("aiso_register.rda")
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

# Дополнительные данные (расположение и адрес лагерей) -----------------
camps <- read.csv2("geography.csv", encoding = "UTF-8")

# Функции --------------------------------------------------------------
CreateRow <- function(x){
  # информация о заезде 
  arrival.date <- as.character(colnames(x)[2]) # дата заезда
  camp.name <- as.character(x[1, 2])           # название лагеря
  names(x) <- as.character(unlist(x[5, ]))     # название колонок
  x <- x[-c(1:5), ]                            # удаление пустых рядов
  x <- x[c(1, 2, 5, 6, 9, 10, 23)]             # удаление ненужных колонок
  x$`Возраст` <- as.numeric(x$`Возраст`)       # ковертация возраста в числовую переменную
  # разбиение массива на подмассивы
  #  commercials <- x %>% filter(`Цель обращения` == "Дополнительные места и услуги для совместного отдыха")
  commercials <- subset(x, grepl("[A-Z]{3}", `Номер заявления`))
  department <- x %>% filter(`Цель обращения` == "Отдых для сирот (совместный отдых)")
  youth <- x %>% filter(`Цель обращения` == "Молодёжный отдых для лиц из числа детей-сирот и детей, оставшихся без попечения родителей, 18-23 лет")
  portal <- subset(x, grepl("[0-9]{4}\\-[0-9]{7}\\-[0-9]{6}\\-[0-9]{7}\\/[0-9]{2}", `Номер заявления`))
  portal <- portal %>% filter(`Цель обращения` != "Отдых для сирот (совместный отдых)")
  portal <- portal %>% filter(`Цель обращения` != "Молодёжный отдых для лиц из числа детей-сирот и детей, оставшихся без попечения родителей, 18-23 лет")
  # подсчёт количества
  commercials.arrived <- nrow(commercials %>% filter(`Заехал` == "Заехал"))
  commercials.kids <- nrow(commercials %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Ребёнок"))
  commercials.adults <- nrow(commercials %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Сопровождающий"))
  commercials.nonarrived <- nrow(commercials %>% filter(`Заехал` == "Не заехал"))
  commercials.nonarrived.kids <- nrow(commercials %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Ребёнок"))
  commercials.nonarrived.adults <- nrow(commercials %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Сопровождающий"))
  
  department.arrived <- nrow(department %>% filter(`Заехал` == "Заехал"))
  department.kids <- nrow(department %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Ребёнок" & `Возраст` < 8))
  department.youth <- nrow(department %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Ребёнок" & `Возраст` > 17))
  department.tutors <- nrow(department %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Воспитатель"))
  department.nonarrived <- nrow(department %>% filter(`Заехал` == "Не заехал"))
  department.nonarrived.kids <- nrow(department %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Ребёнок"  & `Возраст` < 8))
  department.nonarrived.youth <- nrow(department %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Ребёнок"  & `Возраст` > 17))
  department.nonarrived.tutors <- nrow(department %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Воспитатель"))
  
  portal.arrived <- nrow(portal %>% filter(`Заехал` == "Заехал"))
  portal.kids <- portal %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Ребёнок")
  portal.adults <- portal %>% filter(`Заехал` == "Заехал" & `Ребёнок / сопровождающий` == "Сопровождающий")
  
  portal.nonarrived <- nrow(portal %>% filter(`Заехал` == "Не заехал"))
  portal.nonarrived.kids <- nrow(portal %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Ребёнок"))
  portal.nonarrived.adults <- nrow(portal %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Сопровождающий"))
  
  diasbled.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% register.disabled$id, ])
  orphan.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% register.orphans$id, ])
  needy.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% register.needy$id, ])
  
  youth.nonarrived <- youth %>% filter(`Заехал` == "Не заехал")
  youth.arrived <- youth %>% filter(`Заехал` == "Заехал")
  youth.nonarrived <- nrow(youth.nonarrived[youth.nonarrived$`Номер документа` %in% register.youth$id, ])
  youth.arrived <- nrow(youth.arrived[youth.arrived$`Номер документа` %in% register.youth$id, ])
  
  arrived.total <- portal.arrived + youth.arrived + 
    commercials.arrived + department.arrived
  
  nonarrived.total <- portal.nonarrived + youth.nonarrived + 
    commercials.nonarrived + department.nonarrived
  
  # запись в строку
  row <- cbind(camp.name,
               arrival.date,
               portal.arrived, 
               nrow(portal.kids), 
               diasbled.kids,
               orphan.kids, 
               needy.kids, 
               nrow(portal.adults),
               portal.nonarrived, 
               portal.nonarrived.kids,
               portal.nonarrived.adults, 
               youth.arrived, 
               youth.nonarrived,
               commercials.arrived, 
               commercials.kids, 
               commercials.adults,
               commercials.nonarrived,
               commercials.nonarrived.kids, 
               commercials.nonarrived.adults,
               department.arrived, 
               department.kids, 
               department.youth, 
               department.tutors,
               department.nonarrived,
               department.nonarrived.kids, 
               department.nonarrived.youth, 
               department.nonarrived.tutors,
               arrived.total,
               nonarrived.total)
  return(row)
}

Exceler <- function(x){
  file <- read_xlsx(x, sheet = 3)
}

# Create dataset ----
setwd("arrivals_fam/")

files.fam <- list.files(path = "./", recursive = TRUE, pattern = "*.xlsx")
list.fam <- lapply(files.fam, Exceler)

dataset.fam <- lapply(list.fam, CreateRow)
dataset.fam <- data.frame(matrix(unlist(dataset.fam), nrow=length(dataset.fam), byrow=TRUE))
colnames(dataset.fam) <- c("Название лагеря",
                           "Дата заезда",
                           "Совместный - факт",
                           "факт: дети", 
                           "дети-инвалиды", 
                           "дети-сироты", 
                           "дети-малооб.",
                           "факт: сопровождающие",
                           "Совместный - недозаезды",
                           "недозаезды: дети",
                           "недозаезды: сопр.",
                           "Молодёжный - заезды",
                           "Молодёжный - недозаезды",
                           "Доп. путёвки: факт",
                           "Доп. путёвки: факт: дети",
                           "Доп. путёвки: факт: сопр.",
                           "Доп. путёвки: недозаезды",
                           "Доп. путёвки: недозаезды: дети",
                           "Доп. путёвки: недозаезды: сопр",
                           "ДТСЗН: факт",
                           "ДТСЗН: сироты 2-7",
                           "ДТСЗН: сироты 18-23",
                           "ДТСЗН: воспитатели",
                           "ДТСЗН: недозаезды",
                           "ДТСЗН: недозаезды: 2-7",
                           "ДТСЗН: недозаезды: 18-23",
                           "ДТСЗН: недозаезды: воспитатели",
                           "Заезд: всего",
                           "Недозаезд: всего")


convert.cols <- c(3:29)
dataset.fam[ , convert.cols] <- apply(dataset.fam[ , convert.cols], 2,
                    function(x) as.numeric(as.character(x)))
dataset.fam$`Дата заезда` <- as.Date(dataset.fam$`Дата заезда`, format = "%d.%m.%Y")

# Add turnout date
dataset.fam$`Дата выезда` <- dataset.fam$`Дата заезда` + 14

# Geography ----
setwd("../")
camps <- read_xlsx("geography2.xlsx")
dataset.fam$`Зона` <- camps$zone[match(dataset.fam$`Название лагеря`, camps$name)]
dataset.fam$`Регион` <- camps$region[match(dataset.fam$`Название лагеря`, camps$name)]
dataset.fam$`Адрес` <- camps$address[match(dataset.fam$`Название лагеря`, camps$name)]
dataset.fam$`Тип учреждения` <- camps$type[match(dataset.fam$`Название лагеря`, camps$name)]
# Reorder columns in data frame ----
dataset.fam <- dataset.fam[c(1, 31:33, 2, 30, 3:29)]

write.xlsx(dataset.fam, "xlsxs/data_family2019.xlsx")

