GetTraumas2019 <- function(x){
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

GetMedical2019 <- function(x){
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