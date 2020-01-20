setwd("~/aism/2019/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetTraumasFam <- function(x){
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
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]
  date.out <- term[2]
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
load("~/data/camps.rda")
data.trm.fam$region <- camps$region[match(data.trm.fam$camp_name, camps[, 1])]

# Удаление дубликатов
data.trm.fam <- unique(data.trm.fam)

# Подгрузить данные о количестве отдыхающих
load("~/data/data_fam_2019.rda")

data.trm.fam$youth <- fam2019$youth_visits
data.trm.fam$adults <- fam2019$parents_visits + fam2019$add_parents_visits
data.trm.fam$kids <- fam2019$kids_visits + fam2019$add_kids_visits +
  fam2019$dep_visits
data.trm.fam$department <- fam2019$dep_visits
data.trm.fam$disabled <- fam2019$disabled
data.trm.fam$visitors <- fam2019$visits_total

# Расчёт количества обращений на одного отдыхающего
data.trm.fam$per_men <- round(data.trm.fam$total / data.trm.fam$visitors, 2)
data.trm.fam[is.na(data.trm.fam$per_men), ]$per_men <- 0.00

# Атрибуты
attr(data.trm.fam$camp_name, "label") <- "Название организации"
attr(data.trm.fam$date_in, "label") <- "Дата заезда"
attr(data.trm.fam$date_out, "label") <- "Дата выезда"
attr(data.trm.fam$fracture, "label") <- "Переломы"
attr(data.trm.fam$brain_damage, "label") <- "Черепно-мозговые травмы"
attr(data.trm.fam$dislocation_distortion, "label") <- "Вывихи, растяжения"
attr(data.trm.fam$ambustion, "label") <- "Термические и химические ожоги"
attr(data.trm.fam$other, "label") <- "Прочие (царапины, порезы, ушибы)"
attr(data.trm.fam$total, "label") <- "Всего обращений"
attr(data.trm.fam$ins_cases, "label") <- "Всего страховых случаев"
attr(data.trm.fam$region, "label") <- "Регион"
attr(data.trm.fam$youth, "label") <- "Отдыхающие: сироты 18-23 (молодёжный отдых)"
attr(data.trm.fam$adults, "label") <- "Отдыхащие: сопровождающие"
attr(data.trm.fam$kids, "label") <- "Отдыхающие: дети (всего)"
attr(data.trm.fam$department, "label") <- "Отдыхающие: дети-сироты (ДТСЗН)"
attr(data.trm.fam$disabled, "label") <- "Отдыхающие: дети-инвалиды"
attr(data.trm.fam$total, "label") <- "Отдыхающие (всего)"
attr(data.trm.fam$per_men, "label") <- "Кол-во обращений на одного отдыхающего"

# Пересохранить переменную с указанием года
data.med.fam -> trm.fam2019

# Экспорт массива
# save(trm.fam2019, file = "~/data/data_trm_fam2019.rda")
