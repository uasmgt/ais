setwd("~/aism/2019/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetMedicalFam <- function(x){
  camp <- x[1, 2]
  if (ncol(x) == 23 | ncol(x) == 24){ # проверка размерности
    term <- as.character(x[1, 21])    # период отдыха
  } else if (ncol(x) == 16 | ncol(x) == 17 | ncol(x) == 18 | ncol(x) == 19){      # далее см. комментарии выше
    term <- as.character(x[1, 14])
  } else if (ncol(x) == 30){      # далее см. комментарии выше
    term <- as.character(x[1, 26])
  } else if (ncol(x) == 10 | ncol(x) == 13){
    term <- as.character(x[1, 9])
  }  else if (ncol(x) == 11){      # далее см. комментарии выше
    term <- as.character(x[1, 8])
  } else {
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

# Названия заболеваний  ------------------------------------------------
disorders <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "tetter")

# Сборка массива -------------------------------------------------------
files.fam <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.fam <- lapply(files.fam, read.xlsx)
medical.fam <- lapply(list.fam, GetMedicalFam)
data.med.fam <- data.frame(matrix(unlist(medical.fam), 
                                  nrow=length(medical.fam), byrow=TRUE))
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
load("~/data/camps.rda")
data.med.fam$region <- camps$region[match(data.med.fam$camp_name, camps[, 1])]

data.med.fam <- unique(data.med.fam)
# Подгрузить данные о количестве отдыхающих
load("~/data/data_fam_2019.rda")

data.med.fam$youth <- fam2019$youth_visits
data.med.fam$adults <- fam2019$parents_visits + fam2019$add_parents_visits
data.med.fam$kids <- fam2019$kids_visits + fam2019$add_kids_visits +
  fam2019$dep_visits
data.med.fam$department <- fam2019$dep_visits
data.med.fam$disabled <- fam2019$disabled
data.med.fam$visitors <- fam2019$visits_total
data.med.fam$disorders <- fam2019$disorders_total
data.med.fam$mental <-  fam2019$mental
data.med.fam$muscle_skeleton <- fam2019$muscle_skeleton
data.med.fam$dysfunction <- fam2019$dysfunction
data.med.fam$sensorial <- fam2019$sensorial

# Удалить заезды без отдыхающих
data.med.fam <- data.med.fam %>% filter(visitors != 0)

# Рассчитать доли детей разных категорий (разных нарушений) от числа детей
data.med.fam$per_department <- round(data.med.fam$department / data.med.fam$kids, 2)
data.med.fam$per_disabled <- round(data.med.fam$disabled / data.med.fam$kids, 2)
data.med.fam$per_disorders <- round(data.med.fam$disorders / data.med.fam$kids, 2)
data.med.fam$per_mental <- round(data.med.fam$mental / data.med.fam$kids, 2)
data.med.fam$per_muscle_skeleton <- round(data.med.fam$muscle_skeleton / data.med.fam$kids, 2)
data.med.fam$per_dysfunction <- round(data.med.fam$dysfunction / data.med.fam$kids, 2)
data.med.fam$per_sensorial <- round(data.med.fam$sensorial / data.med.fam$kids, 2)

# Рассчитать количество обращений на одного отдыхающего
data.med.fam$per_men <- round(data.med.fam$total / data.med.fam$visitors, 2)

# Атрибуты
attr(data.med.fam$camp_name, "label") <- "Название организации"
attr(data.med.fam$date_in, "label") <- "Дата заезда"
attr(data.med.fam$date_out, "label") <- "Дата выезда"
attr(data.med.fam$infections_infestations, "label") <- "Инфекционные и паразитарные болезни"
attr(data.med.fam$endocrine, "label") <- "Болезни эндокринной системы, нарушения обмена веществ"
attr(data.med.fam$nervous, "label") <- "Болезни нервной системы"
attr(data.med.fam$ocular, "label") <- "Болезни глаза"
attr(data.med.fam$otorhinolaryngology, "label") <- "Оториноларигологические болезни"
attr(data.med.fam$heart, "label") <- "Болезни сердечно-сосудистой системы"
attr(data.med.fam$respiratory, "label") <- "Болезни органов дыхания"
attr(data.med.fam$digestive, "label") <- "Болезни органов пищеварения"
attr(data.med.fam$urogenital, "label") <- "Болезни органов мочеполовой системы"
attr(data.med.fam$intoxication, "label") <- "Отравления"
attr(data.med.fam$heat_apoplexy, "label") <- "Тепловые удары"
attr(data.med.fam$acute, "label") <- "Экстренные и неотложные состояния"
attr(data.med.fam$tetter, "label") <- "Болезни кожи неинфекционные"
attr(data.med.fam$total, "label") <- "Всего обращений"
attr(data.med.fam$ins_cases, "label") <- "Всего страховых случаев"
attr(data.med.fam$region, "label") <- "Регион"
attr(data.med.fam$youth, "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.med.fam$adults, "label") <- "Отдыхащие: сопровождающие"
attr(data.med.fam$kids, "label") <- "Отдыхающие: дети (всего)"
attr(data.med.fam$department, "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.med.fam$disabled, "label") <- "Отдыхающие: дети-инвалиды"
attr(data.med.fam$visitors, "label") <- "Отдыхающие (всего)"
attr(data.med.fam$per_men, "label") <- "Кол-во обращений на одного отдыхающего"
attr(data.med.fam$per_department, "label") <- "Доля детей-сирот (ДТСЗН) от кол-ва детей"
attr(data.med.fam$per_disabled, "label") <- "Доля детей детей-инвалидов"
attr(data.med.fam$per_disorders, "label") <- "Доля детей с нарушениями"
attr(data.med.fam$per_mental, "label") <- "Доля детей с ментальными нарушениями"
attr(data.med.fam$per_muscle_skeleton, "label") <- "Доля детей с нарушениями опорно-двигательного аппарата"
attr(data.med.fam$per_dysfunction, "label") <- "Доля детей с нарушениями функций организма"
attr(data.med.fam$per_sensorial, "label") <- "Доля детей с сенсорными нарушениями"
# Пересохранить переменную с указанием года
data.med.fam -> med.fam2019

# Экспорт массива
# save(med.fam2019, file = "~/data/data_med_fam2019.rda")
