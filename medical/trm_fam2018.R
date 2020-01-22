setwd("~/aism/2018/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetTraumasFam <- function(x){
  camp <- x[1, 2] # название лагеря
  if (ncol(x) >= 30){
    term <- as.character(x[1, 26])
  } else if (ncol(x) == 23){
    term <- as.character(x[1, 20])
  }else if (ncol(x) >= 16){
    term <- as.character(x[1, 14])
  } else if (ncol(x) <= 10){
    term <- as.character(x[1, 8])
  } else {
    sess <- NA
    term <- " - "
  }
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]                           # дата заезда
  date.out <- term[2]                           # дата выезда
  trm01 <- x[4, ncol(x)]
  trm02 <- x[5, ncol(x)]
  trm03 <- x[6, ncol(x)]
  trm04 <- x[7, ncol(x)]
  trm05 <- x[8, ncol(x)]
  trm06 <- x[9, ncol(x)]
  trm.total <- x[10, ncol(x)]
  ins.trm <- x[11, ncol(x)]
  string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                  trm05, trm06, trm.total, ins.trm)
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "aftertroubles", "other")

# Сборка массива -------------------------------------------------------
files.fam <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.trm <- lapply(files.fam, read.xlsx)
trauma.fam <- lapply(list.trm, GetTraumasFam)
data.trm.fam <- data.frame(matrix(unlist(trauma.fam), 
                                  nrow=length(trauma.fam), byrow=TRUE))

colnames(data.trm.fam) <- c("camp_name", "date_in", "date_out",
                            traumas, "total", "ins_cases")
data.trm.fam$date_in <- as.Date(data.trm.fam$date_in, 
                                format = "%d.%m.%Y")
data.trm.fam$date_out <- as.Date(data.trm.fam$date_out, 
                                 format = "%d.%m.%Y")
convert.cols <- c(4:11)
data.trm.fam[ , convert.cols] <- apply(data.trm.fam[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

# Добавление информации о расположении лагерей -------------------------
# Дополнительные данные (расположение и адрес лагерей) -----------------
load("~/data/camps.rda")
data.trm.fam$region <- camps$region[match(data.trm.fam$camp_name, camps[, 1])]
data.trm.fam$duration <- data.trm.fam$date_out - data.trm.fam$date_in + 1

# Подгрузить данные о количестве отдыхающих
load("~/data/data_fam_2018.rda")

data.trm.fam$youth <- fam2018$youth_visits
data.trm.fam$adults <- fam2018$parents_visits + fam2018$add_parents_visits
data.trm.fam$kids <- fam2018$kids_visits + fam2018$add_kids_visits +
  fam2018$dep_visits
data.trm.fam$department <- fam2018$dep_visits
data.trm.fam$disabled <- fam2018$disabled
data.trm.fam$visitors <- fam2018$visits_total
data.trm.fam$disorders <- fam2018$disorders_total
data.trm.fam$mental <-  fam2018$mental
data.trm.fam$muscle_skeleton <- fam2018$muscle_skeleton
data.trm.fam$dysfunction <- fam2018$dysfunction
data.trm.fam$sensorial <- fam2018$sensorial

# Рассчитать доли детей разных категорий (разных нарушений) от числа детей
data.trm.fam$youth <- fam2018$youth_visits
data.trm.fam$adults <- fam2018$parents_visits + fam2018$add_parents_visits
data.trm.fam$kids <- fam2018$kids_visits + fam2018$add_kids_visits +
  fam2018$dep_visits
data.trm.fam$department <- fam2018$dep_visits
data.trm.fam$disabled <- fam2018$disabled
data.trm.fam$visitors <- fam2018$visits_total
data.trm.fam$disorders <- fam2018$disorders_total
data.trm.fam$mental <-  fam2018$mental
data.trm.fam$muscle_skeleton <- fam2018$muscle_skeleton
data.trm.fam$dysfunction <- fam2018$dysfunction
data.trm.fam$sensorial <- fam2018$sensorial

# Удалить заезды без отдыхающих
data.trm.fam <- data.trm.fam %>% filter(visitors != 0)

# Рассчитать количество обращений на одного отдыхающего
data.trm.fam$per_men <- round(data.trm.fam$total / data.trm.fam$visitors, 2)

# Атрибуты
attr(data.trm.fam$camp_name, "label") <- "Название организации"
attr(data.trm.fam$date_in, "label") <- "Дата заезда"
attr(data.trm.fam$date_out, "label") <- "Дата выезда"
attr(data.trm.fam$fracture, "label") <- "Переломы"
attr(data.trm.fam$brain_damage, "label") <- "Черепно-мозговые травмы"
attr(data.trm.fam$dislocation_distortion, "label") <- "Вывихи, растяжения"
attr(data.trm.fam$ambustion, "label") <- "Термические и химические ожоги"
attr(data.trm.fam$aftertroubles, "label") <- "Последствия травм и отравлений"
attr(data.trm.fam$other, "label") <- "Прочие (царапины, порезы, ушибы)"
attr(data.trm.fam$total, "label") <- "Всего обращений"
attr(data.trm.fam$ins_cases, "label") <- "Всего страховых случаев"
attr(data.trm.fam$region, "label") <- "Регион"
attr(data.trm.fam$duration, "label") <- "Продолжительность заезда"
attr(data.trm.fam$youth, "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.trm.fam$adults, "label") <- "Отдыхащие: сопровождающие"
attr(data.trm.fam$kids, "label") <- "Отдыхающие: дети (всего)"
attr(data.trm.fam$department, "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.trm.fam$disabled, "label") <- "Отдыхающие: дети-инвалиды"
attr(data.trm.fam$total, "label") <- "Отдыхающие (всего)"
attr(data.trm.fam$per_men, "label") <- "Кол-во обращений на одного отдыхающего"
attr(data.trm.fam$per_department, "label") <- "Доля детей-сирот (ДТСЗН) от кол-ва детей"
attr(data.trm.fam$per_disabled, "label") <- "Доля детей детей-инвалидов"
attr(data.trm.fam$per_disorders, "label") <- "Доля детей с нарушениями"
attr(data.trm.fam$per_mental, "label") <- "Доля детей с ментальными нарушениями"
attr(data.trm.fam$per_muscle_skeleton, "label") <- "Доля детей с нарушениями опорно-двигательного аппарата"
attr(data.trm.fam$per_dysfunction, "label") <- "Доля детей с нарушениями функций организма"
attr(data.trm.fam$per_sensorial, "label") <- "Доля детей с сенсорными нарушениями"
attr(data.trm.fam$disorders, "label") <- "Кол-во детей с нарушениями"
attr(data.trm.fam$mental, "label") <- "Кол-во детей с ментальными нарушениями"
attr(data.trm.fam$muscle_skeleton, "label") <- "Кол-во детей с нарушениями опорно-двигательного аппарата"
attr(data.trm.fam$dysfunction, "label") <- "Кол-во детей с нарушениями функций организма"
attr(data.trm.fam$sensorial, "label") <- "Кол-во детей с сенсорными нарушениями"

# Пересохранить переменную с указанием года
data.trm.fam -> trm.fam201

# Экспорт массива
# save(trm.fam2018, file = "~/data/data_trm_fam2018.rda")