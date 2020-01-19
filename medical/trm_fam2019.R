setwd("~/aism/2019/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetTraumasFam <- function(x){
  camp <-  x[1, 2]
  term <-  as.character(x[1, 14])
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]
  date.out <- term[2]
  if (ncol(x) == 17) {
    trm01 <- x[4, 17]
    trm02 <- x[5, 17]
    trm03 <- x[6, 17]
    trm04 <- x[7, 17]
    trm05 <- x[8, 17]
    trm.total <- x[9, 17]
    ins.trm <- x[10, 17]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 16) {
    trm01 <- x[4, 16]
    trm02 <- x[5, 16]
    trm03 <- x[6, 16]
    trm04 <- x[7, 16]
    trm05 <- x[8, 16]
    trm.total <- x[9, 16]
    ins.trm <- x[10, 16]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 18) {
    trm01 <- x[4, 18]
    trm02 <- x[5, 18]
    trm03 <- x[6, 18]
    trm04 <- x[7, 18]
    trm05 <- x[8, 18]
    trm.total <- x[9, 18]
    ins.trm <- x[10, 18]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 19) {
    trm01 <- x[4, 19]
    trm02 <- x[5, 19]
    trm03 <- x[6, 19]
    trm04 <- x[7, 19]
    trm05 <- x[8, 19]
    trm.total <- x[9, 19]
    ins.trm <- x[10, 19]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 23) {
    term <-  as.character(x[1, 20])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    trm01 <- x[4, 23]
    trm02 <- x[5, 23]
    trm03 <- x[6, 23]
    trm04 <- x[7, 23]
    trm05 <- x[8, 23]
    trm.total <- x[9, 23]
    ins.trm <- x[10, 23]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 30) {
    term <-  as.character(x[1, 26])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    trm01 <- x[4, 30]
    trm02 <- x[5, 30]
    trm03 <- x[6, 30]
    trm04 <- x[7, 30]
    trm05 <- x[8, 30]
    trm.total <- x[9, 30]
    ins.trm <- x[10, 30]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else if (ncol(x) == 13) {
    term <-  as.character(x[1, 11])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    trm01 <- x[4, 13]
    trm02 <- x[5, 13]
    trm03 <- x[6, 13]
    trm04 <- x[7, 13]
    trm05 <- x[8, 13]
    trm.total <- x[9, 13]
    ins.trm <- x[10, 13]
    string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                    trm05, trm.total, ins.trm)
  } else {
    string <- cbind(rep(NA, 18))
  }
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "other")

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
convert.cols <- c(4:10)
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
attr(data.trm.fam[, 8], "label") <- "Прочие (царапины, порезы, ушибы)"
attr(data.trm.fam[, 9], "label") <- "Всего обращений"
attr(data.trm.fam[, 10], "label") <- "Всего страховых случаев"
attr(data.trm.fam[, 11], "label") <- "Регион"
attr(data.trm.fam[, 12], "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.trm.fam[, 13], "label") <- "Отдыхащие: сопровождающие"
attr(data.trm.fam[, 14], "label") <- "Отдыхающие: дети (всего)"
attr(data.trm.fam[, 15], "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.trm.fam[, 16], "label") <- "Отдыхающие: дети-инвалиды"
attr(data.trm.fam[, 17], "label") <- "Отдыхающие (всего)"

# Экспорт массива
# save(data.trm.fam, file = "~/aism/2019/data_trm_fam2019.rda")
