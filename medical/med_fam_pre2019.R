setwd("~/aism/2018/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetMedicalFam <- function(x){
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
  bol01 <- x[16, ncol(x)]
  bol02 <- x[17, ncol(x)]
  bol03 <- x[18, ncol(x)]
  bol04 <- x[19, ncol(x)]
  bol05 <- x[20, ncol(x)]
  bol06 <- x[21, ncol(x)]
  bol07 <- x[22, ncol(x)]
  bol08 <- x[23, ncol(x)]
  bol09 <- x[24, ncol(x)]
  bol10 <- x[25, ncol(x)]
  bol11 <- x[26, ncol(x)]
  bol12 <- x[27, ncol(x)]
  bol13 <- x[28, ncol(x)]
  bol.total <- x[29, ncol(x)]
  ins.total <- x[30, ncol(x)]
  string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                  bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                  bol12, bol13, bol.total, ins.total)
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
disorders <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "other")

# Сборка массива -------------------------------------------------------
files.fam <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.fam <- lapply(files.fam, read.xlsx)
medical.fam <- lapply(list.fam, GetMedicalFam)
data.med.fam <- data.frame(matrix(unlist(medical.fam), 
                                  nrow=length(medical.fam), byrow=TRUE))
# data.med.fam <- na.omit(data.med.fam)
colnames(data.med.fam) <- c("camp_name", "date_in", "date_out",
                            disorders, "total", "ins_cases")
data.med.fam$date_in <- as.Date(data.med.fam$date_in, 
                                format = "%d.%m.%Y")
data.med.fam$date_out <- as.Date(data.med.fam$date_out, 
                                 format = "%d.%m.%Y")
convert.cols <- c(4:18)
data.med.fam[ , convert.cols] <- apply(data.med.fam[ , convert.cols], 2,
                                       function(x) as.numeric(as.character(x)))

# Добавление информации о расположении лагерей -------------------------
# Дополнительные данные (расположение и адрес лагерей) -----------------
load("~/aism/camps.rda")
data.med.fam$region <- camps$region[match(data.med.fam$camp_name, camps[, 1])]

# Подгрузить данные о количестве отдыхающих
load("~/aism/2019/data_fam.rda")

data.med.fam$youth <- data.fam$youth_visits
data.med.fam$adults <- data.fam$parents_visits + data.fam$add_parents_visits
data.med.fam$kids <- data.fam$kids_visits + data.fam$add_kids_visits +
  data.fam$dep_visits
data.med.fam$department <- data.fam$dep_visits
data.med.fam$disabled <- data.fam$disabled
data.med.fam$visitors <- data.fam$visits_total

# Атрибуты
attr(data.med.fam[, 1], "label") <- "Название организации"
attr(data.med.fam[, 2], "label") <- "Дата заезда"
attr(data.med.fam[, 3], "label") <- "Дата выезда"
attr(data.med.fam[, 4], "label") <- "Инфекционные и паразитарные болезни"
attr(data.med.fam[, 5], "label") <- "Болезни эндокринной системы, нарушения обмена веществ"
attr(data.med.fam[, 6], "label") <- "Болезни нервной системы"
attr(data.med.fam[, 7], "label") <- "Болезни глаза"
attr(data.med.fam[, 8], "label") <- "Оториноларигологические болезни"
attr(data.med.fam[, 9], "label") <- "Болезни сердечно-сосудистой системы"
attr(data.med.fam[, 10], "label") <- "Болезни органов дыхания"
attr(data.med.fam[, 11], "label") <- "Болезни органов пищеварения"
attr(data.med.fam[, 12], "label") <- "Болезни органов мочеполовой системы"
attr(data.med.fam[, 13], "label") <- "Отравления"
attr(data.med.fam[, 14], "label") <- "Тепловые удары"
attr(data.med.fam[, 15], "label") <- "Экстренные и неотложные состояния"
attr(data.med.fam[, 16], "label") <- "Болезни кожи неинфекционные"
attr(data.med.fam[, 17], "label") <- "Всего обращений"
attr(data.med.fam[, 18], "label") <- "Всего страховых случаев"
attr(data.med.fam[, 19], "label") <- "Регион"
attr(data.med.fam[, 20], "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.med.fam[, 21], "label") <- "Отдыхащие: сопровождающие"
attr(data.med.fam[, 22], "label") <- "Отдыхающие: дети (всего)"
attr(data.med.fam[, 23], "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.med.fam[, 24], "label") <- "Отдыхающие: дети-инвалиды"
attr(data.med.fam[, 25], "label") <- "Отдыхающие (всего)"


# Экспорт массива
# save(data.med.fam, file = "~/aism/2018/data_medical_fam2018.rda")
