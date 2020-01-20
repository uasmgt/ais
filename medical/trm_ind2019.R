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
load("~/data/camps.rda")
data.trm.ind$region <- camps$region[match(data.trm.ind$camp_name, camps[, 1])]

# Удаление дубликатов
data.trm.ind <- unique(data.trm.ind)

# Подгрузить данные о количестве отдыхающих
load("~/data/data_ind_2019.rda")

data.trm.ind$kids <- ind2019$fact_total
data.trm.ind$kids_vouchers <- ind2019$fact_vouchers
data.trm.ind$kids_dep <- ind2019$fact_dep
data.trm.ind$disabled <- ind2019$disabled

convert.cols <- c(4:10, 12:15)
data.trm.ind[ , convert.cols] <- apply(data.trm.ind[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

data.trm.ind$per_men <- round(data.trm.ind$total / data.trm.ind$kids, 2)
data.trm.ind[is.nan(data.trm.ind$per_men), ]$per_men <- 0.00
data.trm.ind[is.infinite(data.trm.ind$per_men), ]$per_men <- 0.00

# Атрибуты
attr(data.trm.ind$camp_name, "label") <- "Название организации"
attr(data.trm.ind$date_in, "label") <- "Дата заезда"
attr(data.trm.ind$date_out, "label") <- "Дата выезда"
attr(data.trm.ind$fracture, "label") <- "Переломы"
attr(data.trm.ind$brain_damage, "label") <- "Черепно-мозговые травмы"
attr(data.trm.ind$dislocation_distortion, "label") <- "Вывихи, растяжения"
attr(data.trm.ind$ambustion, "label") <- "Термические и химические ожоги"
attr(data.trm.ind$other, "label") <- "Прочие (царапины, порезы, ушибы)"
attr(data.trm.ind$total, "label") <- "Всего обращений"
attr(data.trm.ind$ins_cases, "label") <- "Всего страховых случаев"
attr(data.trm.ind$region, "label") <- "Регион"
attr(data.trm.ind$kids, "label") <- "Количество отдыхающих"
attr(data.trm.ind$kids_vouchers, "label") <- "Отдыхащие: по путёвкам"
attr(data.trm.ind$kids_dep, "label") <- "Отдыхающие: по спискам ДТСЗН"
attr(data.trm.ind$disabled, "label") <- "Отдыхающие: инвалиды (по путёвкам)"
attr(data.trm.ind$per_men, "label") <- "Кол-во обращений на одного отдыхающего"

# Экспорт массива
# save(data.trm.ind, file = "~/data/data_trm_ind2019.rda")
