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
  } else if (ncol(x) == 18) {
    bol01 <- x[15, 18]
    bol02 <- x[16, 18]
    bol03 <- x[17, 18]
    bol04 <- x[18, 18]
    bol05 <- x[19, 18]
    bol06 <- x[20, 18]
    bol07 <- x[21, 18]
    bol08 <- x[22, 18]
    bol09 <- x[23, 18]
    bol10 <- x[24, 18]
    bol11 <- x[25, 18]
    bol12 <- x[26, 18]
    bol13 <- x[27, 18]
    bol.total <- x[28, 18]
    ins.total <- x[29, 18]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else if (ncol(x) == 19) {
    bol01 <- x[15, 19]
    bol02 <- x[16, 19]
    bol03 <- x[17, 19]
    bol04 <- x[18, 19]
    bol05 <- x[19, 19]
    bol06 <- x[20, 19]
    bol07 <- x[21, 19]
    bol08 <- x[22, 19]
    bol09 <- x[23, 19]
    bol10 <- x[24, 19]
    bol11 <- x[25, 19]
    bol12 <- x[26, 19]
    bol13 <- x[27, 19]
    bol.total <- x[28, 19]
    ins.total <- x[29, 19]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else if (ncol(x) == 23) {
    term <-  as.character(x[1, 20])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    bol01 <- x[15, 23]
    bol02 <- x[16, 23]
    bol03 <- x[17, 23]
    bol04 <- x[18, 23]
    bol05 <- x[19, 23]
    bol06 <- x[20, 23]
    bol07 <- x[21, 23]
    bol08 <- x[22, 23]
    bol09 <- x[23, 23]
    bol10 <- x[24, 23]
    bol11 <- x[25, 23]
    bol12 <- x[26, 23]
    bol13 <- x[27, 23]
    bol.total <- x[28, 23]
    ins.total <- x[29, 23]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else if (ncol(x) == 30) {
    term <-  as.character(x[1, 26])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    bol01 <- x[15, 30]
    bol02 <- x[16, 30]
    bol03 <- x[17, 30]
    bol04 <- x[18, 30]
    bol05 <- x[19, 30]
    bol06 <- x[20, 30]
    bol07 <- x[21, 30]
    bol08 <- x[22, 30]
    bol09 <- x[23, 30]
    bol10 <- x[24, 30]
    bol11 <- x[25, 30]
    bol12 <- x[26, 30]
    bol13 <- x[27, 30]
    bol.total <- x[28, 30]
    ins.total <- x[29, 30]
    string <- cbind(camp, date.in, date.out, bol01, bol02, bol03, bol04, 
                    bol05, bol06, bol07, bol08, bol09, bol10, bol11, 
                    bol12, bol13, bol.total, ins.total)
  } else if (ncol(x) == 13) {
    term <-  as.character(x[1, 11])
    term <- unlist(strsplit(term, split = " - "))
    date.in  <- term[1]
    date.out <- term[2]
    bol01 <- x[15, 13]
    bol02 <- x[16, 13]
    bol03 <- x[17, 13]
    bol04 <- x[18, 13]
    bol05 <- x[19, 13]
    bol06 <- x[20, 13]
    bol07 <- x[21, 13]
    bol08 <- x[22, 13]
    bol09 <- x[23, 13]
    bol10 <- x[24, 13]
    bol11 <- x[25, 13]
    bol12 <- x[26, 13]
    bol13 <- x[27, 13]
    bol.total <- x[28, 13]
    ins.total <- x[29, 13]
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
