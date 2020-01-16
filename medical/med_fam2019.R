setwd("~/aism/2019/arrivals_fam/")

library(openxlsx)
library(dplyr)

# Функции --------------------------------------------------------------
GetMedicalFam <- function(x){
  camp <-  x[1, 2]
  term <-  as.character(x[1, 14])
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]
  date.out <- term[2]
  if (ncol(x) == 17) {
    bol01 <- x[15, 17]
    bol02 <- x[16, 17]
    bol03 <- x[17, 17]
    bol04 <- x[18, 17]
    bol05 <- x[19, 17]
    bol06 <- x[20, 17]
    bol07 <- x[21, 17]
    bol08 <- x[22, 17]
    bol09 <- x[23, 17]
    bol10 <- x[24, 17]
    bol11 <- x[25, 17]
    bol12 <- x[26, 17]
    bol13 <- x[27, 17]
    bol.total <- x[28, 17]
    ins.total <- x[29, 17]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else if (ncol(x) == 16) {
    bol01 <- x[15, 16]
    bol02 <- x[16, 16]
    bol03 <- x[17, 16]
    bol04 <- x[18, 16]
    bol05 <- x[19, 16]
    bol06 <- x[20, 16]
    bol07 <- x[21, 16]
    bol08 <- x[22, 16]
    bol09 <- x[23, 16]
    bol10 <- x[24, 16]
    bol11 <- x[25, 16]
    bol12 <- x[26, 16]
    bol13 <- x[27, 16]
    bol.total <- x[28, 16]
    ins.total <- x[29, 16]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else {
    string <- cbind(rep(NA, 18))
  }
  return(string)
}

# Названия заболеваний и травм -----------------------------------------
disorders <- c("infections_infestations", "endocrine", "nervous",
               "ocular", "otorhinolaryngology", "heart", "respiratory",
               "digestive", "urogenital", "intoxication", 
               "heat_apoplexy", "acute", "tetter")
traumas <- c("fracture", "brain_damage", "dislocation_distortion",
             "ambustion", "other")

# Сборка массива -------------------------------------------------------
files.fam <- list.files(path = "./", recursive = TRUE, 
                        pattern = "*.xlsx")
list.fam <- lapply(files.fam, read.xlsx)
medical.fam <- lapply(list.fam, GetMedicalFam)
data.med.fam <- data.frame(matrix(unlist(medical.fam), 
                                  nrow=length(medical.fam), byrow=TRUE))
data.med.fam <- na.omit(data.med.fam)
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
