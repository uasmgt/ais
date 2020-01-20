setwd("~/aism/2019/arrivals_ind/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetTraumaInd <- function(x){
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
  trm01 <- x[4, ncol(x)]
  trm02 <- x[5, ncol(x)]
  trm03 <- x[6, ncol(x)]
  trm04 <- x[7, ncol(x)]
  trm05 <- x[8, ncol(x)]
  trm.total <- x[9, ncol(x)]
  ins.trm <- x[10, ncol(x)]
  string <- cbind(camp, date.in, date.out, trm01, trm02, trm03, trm04, 
                  trm05, trm.total, ins.trm)
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "other")

# Сборка массива -------------------------------------------------------
files.ind <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.ind <- lapply(files.ind, read.xlsx)
traumas.ind <- lapply(list.ind, GetTraumaInd)
data.trm.ind <- data.frame(matrix(unlist(traumas.ind), 
                                  nrow=length(traumas.ind), byrow=TRUE))
# data.med.fam <- na.omit(data.med.fam)
colnames(data.trm.ind) <- c("camp_name", "date_in", "date_out",
                            traumas, "total", "ins_cases")
data.trm.ind$date_in <- as.Date(data.trm.ind$date_in, 
                                format = "%d.%m.%Y")
data.trm.ind$date_out <- as.Date(data.trm.ind$date_out, 
                                 format = "%d.%m.%Y")
convert.cols <- c(4:10)
data.trm.ind[ , convert.cols] <- apply(data.trm.ind[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

# Добавление информации о расположении лагерей -------------------------
# Дополнительные данные (расположение и адрес лагерей) -----------------
load("~/aism/camps.rda")
data.trm.ind$region <- camps$region[match(data.trm.ind$camp_name, camps[, 1])]

# Удаление дубликатов
data.trm.ind <- unique(data.trm.ind)

# Подгрузить данные о количестве отдыхающих
load("~/aism/2019/data_ind.rda")

data.trm.ind$kids <- data.ind$fact_total
data.trm.ind$kids_vouchers <- data.ind$fact_vouchers
data.trm.ind$kids_dep <- data.ind$fact_dep
data.trm.ind$disabled <- data.ind$disabled

convert.cols <- c(4:10, 12:15)
data.trm.ind[ , convert.cols] <- apply(data.trm.ind[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

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
attr(data.trm.ind[, 12], "label") <- "Количество отдыхающих"
attr(data.trm.ind[, 13], "label") <- "Отдыхащие: по путёвкам"
attr(data.trm.ind[, 14], "label") <- "Отдыхающие: по спискам ДТСЗН"
attr(data.trm.ind[, 15], "label") <- "Отдыхающие: инвалиды (по путёвкам)"

# Экспорт массива
# save(data.trm.ind, file = "~/aism/2019/data_trm_ind2019.rda")
