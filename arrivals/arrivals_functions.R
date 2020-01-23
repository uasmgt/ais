GetInfo <- function(x){
  x <- read.xlsx(x, sheet = 1)
  camp <- x[1, 2]
  if (ncol(x) >= 30){
    term <- as.character(x[1, 26])
  } else if (ncol(x) >= 23){
    term <- as.character(x[1, 20])
  } else if (ncol(x) >= 16){
    term <- as.character(x[1, 14])
  } else if (ncol(x) <= 15){
    term <- as.character(x[1, 8])
  } else {
    term <- " - "
  }
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]
  date.out <- term[2]
  string <- cbind(camp, date.in, date.out)
}

GetSession <- function(x){
  x <- read.xlsx(x, sheet = 1)
  if (ncol(x) >= 23){
    session <- x[1, 14]
  } else if (ncol(x) >= 16){
    session <- x[1, 10]
  } else if (ncol(x) <= 15){
    session <- x[1, 6]
  } else {
    session <- ""
  }
  return(session) # возврат значения
}

GetStaff <- function(x){
  x <- read.xlsx(x, sheet = 2)
  if (ncol(x) == 13) { #  проверка заполнения данных
    names(x) <- as.character(unlist(x[3, ])) # чтение заголовков столбцов
    x <- x[-c(1:3), ] # удаление пустых строк
    tutors.num <- nrow(x %>% filter(`Должность` == "Воспитатель")) # подсчёт воспитателей
    tutors.num <- cbind(tutors.num)} else { # запись нолей для незаполненных листов
      tutors.num <- 0
    }
  return(tutors.num) # возврат значения
}

CountInd <- function(x){
  # Подготовка xlsx-файла к анализу
  names(x) <- as.character(unlist(x[4, ]))     # чтение заголовков столбцов
  x <- x[-c(1:4), ]                            # удаление пустых строк
  x <- x[c(1, 2, 3, 5, 7, 8, 10, 11, 12, 13, 27)] # удаление лишних столбцов
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
  # Подсчёт заехавших по нарушениям
  mental <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                (`Вид ограничения` == "Ментальные, психические и неврологические нарушения" |
                                   `Вид ограничения` == "Ментальные (восприятия, внимания, памяти, мышления, речи, эмоций, воли и др.)" | 
                                   `Вид ограничения` == "Ментальные, психические и неврологические нарушения")))
  muscle.skeleton <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                         (`Вид ограничения` == "Нарушения опорно-двигательного аппарата" |
                                            `Вид ограничения` == "Нарушения опорно-двигательного аппарата (передвигается с помощью инвалидного кресла-коляски)" | 
                                            `Вид ограничения` == "Нарушения опорно-двигательного аппарата (передвигается самостоятельно или с опорой: ходунки, трость, краб)" |
                                            `Вид ограничения` == "Нарушения статодинамической функции (двигательной)")))
  dysfunction <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                     (`Вид ограничения` == "Нарушения функций систем организма" | 
                                        `Вид ограничения` == "Нарушения функций кровообращения, дыхания, пищеварения, выделения, обмена веществ и энергии, внутренней секреции и др.")))
  sensorial <- nrow(x %>% filter(`Заехал` == "Заехал" &
                                   (`Вид ограничения` == "Сенсорные нарушения" |
                                      `Вид ограничения` == "Сенсорные (зрения, слуха, обоняния, осязания и др.)")))
  disorders.total <- nrow(x %>% filter(`Заехал` == "Заехал" & `Вид ограничения` != "-"))
  # Запись подсчитанных данных в строку
  row <- cbind(arrivals.por.num, disabled, orphans, needy,
               non.arrivals.por.num, non.arrivals.by.denial,
               arrivals.dep, non.arrivals.dep,
               arrivals.sum, mental, muscle.skeleton, dysfunction,
               sensorial, disorders.total, non.arrivals.sum)
}

CountFam <- function(x){
  x <- read.xlsx(x, sheet = 3)
  names(x) <- as.character(unlist(x[4, ]))     # название колонок
  x <- x[-c(1:4), ]                            # удаление пустых рядов
  x <- x[c(1, 2, 3, 5, 6, 9, 10, 23)]             # удаление ненужных колонок
  x$`Возраст` <- as.numeric(x$`Возраст`)       # ЗАЧЕМ НАМ ВОЗРАСТ?!
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
  
  mental <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                `Вид ограничения` == "Ментальные, психические и неврологические нарушения"))
  muscle.skeleton <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                         `Вид ограничения` == "Нарушения опорно-двигательного аппарата"))
  dysfunction <- nrow(x %>% filter(`Заехал` == "Заехал" & 
                                     `Вид ограничения` == "Нарушения функций систем организма"))
  sensorial <- nrow(x %>% filter(`Заехал` == "Заехал" &
                                   `Вид ограничения` == "Сенсорные нарушения"))
  disorders.total <- nrow(x %>% filter(`Заехал` == "Заехал" & `Вид ограничения` != "-"))
  
  portal.nonarrived <- nrow(portal %>% filter(`Заехал` == "Не заехал"))
  portal.nonarrived.kids <- nrow(portal %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Ребёнок"))
  portal.nonarrived.adults <- nrow(portal %>% filter(`Заехал` == "Не заехал"  & `Ребёнок / сопровождающий` == "Сопровождающий"))
  
  diasbled.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% aiso.disabled$id, ])
  orphan.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% aiso.orphans$id, ])
  needy.kids <- nrow(portal.kids[portal.kids$`Номер документа` %in% aiso.needy$id, ])
  
  youth.nonarrived <- youth %>% filter(`Заехал` == "Не заехал")
  youth.arrived <- youth %>% filter(`Заехал` == "Заехал")
  youth.nonarrived <- nrow(youth.nonarrived[youth.nonarrived$`Номер документа` %in% aiso.youth$id, ])
  youth.arrived <- nrow(youth.arrived[youth.arrived$`Номер документа` %in% aiso.youth$id, ])
  
  arrived.total <- portal.arrived + youth.arrived + 
    commercials.arrived + department.arrived
  
  nonarrived.total <- portal.nonarrived + youth.nonarrived + 
    commercials.nonarrived + department.nonarrived
  
  # запись в строку
  row <- cbind(portal.arrived, nrow(portal.kids), diasbled.kids,
               orphan.kids, needy.kids, nrow(portal.adults),
               portal.nonarrived, portal.nonarrived.kids, portal.nonarrived.adults, 
               youth.arrived, youth.nonarrived, commercials.arrived, 
               commercials.kids, commercials.adults, commercials.nonarrived,
               commercials.nonarrived.kids, commercials.nonarrived.adults,
               department.arrived, department.kids, department.youth, 
               department.tutors, department.nonarrived, 
               department.nonarrived.kids, department.nonarrived.youth, 
               department.nonarrived.tutors, arrived.total,
               mental, muscle.skeleton, dysfunction, sensorial,
               disorders.total, nonarrived.total)
  return(row)
}


ReadSheets <- function(x){
  n.sheets <- getSheetNames(x) # сбор заголовков листов в xlsx-файле
  l.sheets <- as.list(rep(NA, c(length(n.sheets)))) # создание листа с листами (ха-ха)
  names(l.sheets) <- n.sheets                       # чтение листов
  for (i in 1:length(n.sheets)) {                   # обработка xlsx-файлов
    l.sheets[[i]] <- read.xlsx(x, sheet = i)
  }
  l.sheets <- l.sheets[-c(1, 2)]                    # удаление листов со сведениями о заезде и пед. персонале
  l.sheets <- Filter(function(x) {ncol(x) == 30}, l.sheets) # обработка листов с отрядами
  data <- lapply(l.sheets, CountInd)               # создание по данным отрядов заезда
  data <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE)) # табличное представление строки
  string <- data.frame(colSums(data)) # суммирование данных по отрядам
  return(string)
}