setwd("~/aism/2019/arrivals_ind/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetMedicalInd <- function(x){
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
  bol01 <- x[15, ncol(x)]
  bol02 <- x[16, ncol(x)]
  bol03 <- x[17, ncol(x)]
  bol04 <- x[18, ncol(x)]
  bol05 <- x[19, ncol(x)]
  bol06 <- x[20, ncol(x)]
  bol07 <- x[21, ncol(x)]
  bol08 <- x[22, ncol(x)]
  bol09 <- x[23, ncol(x)]
  bol10 <- x[24, ncol(x)]
  bol11 <- x[25, ncol(x)]
  bol12 <- x[26, ncol(x)]
  bol13 <- x[27, ncol(x)]
  bol.total <- x[28, ncol(x)]
  ins.total <- x[29, ncol(x)]
  string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                  bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                  bol12, bol13, bol.total, ins.total)
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
disorders <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "tetter")

# Сборка массива -------------------------------------------------------
files.ind <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.ind <- lapply(files.ind, read.xlsx)
medical.ind <- lapply(list.ind, GetMedicalInd)
data.med.ind <- data.frame(matrix(unlist(medical.ind), 
                                  nrow=length(medical.ind), byrow=TRUE))
# data.med.fam <- na.omit(data.med.fam)
colnames(data.med.ind) <- c("camp_name", "date_in", "date_out",
                            disorders, "total", "ins_cases")
data.med.ind$date_in <- as.Date(data.med.ind$date_in, 
                                format = "%d.%m.%Y")
data.med.ind$date_out <- as.Date(data.med.ind$date_out, 
                                 format = "%d.%m.%Y")
convert.cols <- c(4:18)
data.med.ind[ , convert.cols] <- apply(data.med.ind[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

# Добавление информации о расположении лагерей -------------------------
# Дополнительные данные (расположение и адрес лагерей) -----------------
load("~/aism/camps.rda")
data.med.ind$region <- camps$region[match(data.med.ind$camp_name, camps[, 1])]

# Удаление дубликатов
data.med.ind <- unique(data.med.ind)

# Подгрузить данные о количестве отдыхающих
load("~/aism/2019/data_ind.rda")

data.med.ind$kids <- data.ind$fact_total
data.med.ind$kids_vouchers <- data.ind$fact_vouchers
data.med.ind$kids_dep <- data.ind$fact_dep
data.med.ind$disabled <- data.ind$disabled

# Атрибуты
attr(data.med.ind[, 1], "label") <- "Название организации"
attr(data.med.ind[, 2], "label") <- "Дата заезда"
attr(data.med.ind[, 3], "label") <- "Дата выезда"
attr(data.med.ind[, 4], "label") <- "Инфекционные и паразитарные болезни"
attr(data.med.ind[, 5], "label") <- "Болезни эндокринной системы, нарушения обмена веществ"
attr(data.med.ind[, 6], "label") <- "Болезни нервной системы"
attr(data.med.ind[, 7], "label") <- "Болезни глаза"
attr(data.med.ind[, 8], "label") <- "Оториноларигологические болезни"
attr(data.med.ind[, 9], "label") <- "Болезни сердечно-сосудистой системы"
attr(data.med.ind[, 10], "label") <- "Болезни органов дыхания"
attr(data.med.ind[, 11], "label") <- "Болезни органов пищеварения"
attr(data.med.ind[, 12], "label") <- "Болезни органов мочеполовой системы"
attr(data.med.ind[, 13], "label") <- "Отравления"
attr(data.med.ind[, 14], "label") <- "Тепловые удары"
attr(data.med.ind[, 15], "label") <- "Экстренные и неотложные состояния"
attr(data.med.ind[, 16], "label") <- "Болезни кожи неинфекционные"
attr(data.med.ind[, 17], "label") <- "Всего обращений"
attr(data.med.ind[, 18], "label") <- "Всего страховых случаев"
attr(data.med.ind[, 19], "label") <- "Регион"
attr(data.med.ind[, 20], "label") <- "Количество отдыхающих"
attr(data.med.ind[, 21], "label") <- "Отдыхащие: по путёвкам"
attr(data.med.ind[, 22], "label") <- "Отдыхающие: по спискам ДТСЗН"
attr(data.med.ind[, 23], "label") <- "Отдыхающие: инвалиды (по путёвкам)"


# Экспорт массива
# save(data.med.ind, file = "~/aism/2019/data_medical_ind2019.rda")
