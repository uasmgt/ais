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
load("~/aism/camps.rda")
data.trm.fam$region <- camps$region[match(data.trm.fam$camp_name, camps[, 1])]

# Подгрузить данные о количестве отдыхающих
load("~/aism/2019/data_fam.rda")

data.trm.fam$youth <- data.fam$youth_visits
data.trm.fam$adults <- data.fam$parents_visits + data.fam$add_parents_visits
data.trm.fam$kids <- data.fam$kids_visits + data.fam$add_kids_visits +
  data.fam$dep_visits
data.trm.fam$department <- data.fam$dep_visits
data.trm.fam$disabled <- data.fam$disabled
data.trm.fam$visitors <- data.fam$visits_total

# Атрибуты
attr(data.trm.fam[, 1], "label") <- "Название организации"
attr(data.trm.fam[, 2], "label") <- "Дата заезда"
attr(data.trm.fam[, 3], "label") <- "Дата выезда"
attr(data.trm.fam[, 4], "label") <- "Переломы"
attr(data.trm.fam[, 5], "label") <- "Черепно-мозговые травмы"
attr(data.trm.fam[, 6], "label") <- "Вывихи, растяжения"
attr(data.trm.fam[, 7], "label") <- "Термические и химические ожоги"
attr(data.trm.fam[, 8], "label") <- "Последствия травм, отравлений"
attr(data.trm.fam[, 9], "label") <- "Прочие (царапины, порезы, ушибы)"
attr(data.trm.fam[, 10], "label") <- "Всего обращений"
attr(data.trm.fam[, 11], "label") <- "Всего страховых случаев"
attr(data.trm.fam[, 12], "label") <- "Регион"
attr(data.trm.fam[, 13], "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.trm.fam[, 14], "label") <- "Отдыхащие: сопровождающие"
attr(data.trm.fam[, 15], "label") <- "Отдыхающие: дети (всего)"
attr(data.trm.fam[, 16], "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.trm.fam[, 17], "label") <- "Отдыхающие: дети-инвалиды"
attr(data.trm.fam[, 18], "label") <- "Отдыхающие (всего)"

# Экспорт массива
# save(data.trm.fam, file = "~/aism/2019/data_trm_fam2019.rda")

