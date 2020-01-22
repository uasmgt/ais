GetInfo <- function(x){
  camp <- x[1, 2]
  if (ncol(x) >= 30){
    term <- as.character(x[1, 26])
  } else if (ncol(x) >= 23){
    term <- as.character(x[1, 20])
  } else if (ncol(x) >= 16){
    term <- as.character(x[1, 14])
  } else if (ncol(x) <= 14){
    term <- as.character(x[1, 8])
  } else {
    term <- " - "
  }
  term <- unlist(strsplit(term, split = " - "))
  date.in  <- term[1]
  date.out <- term[2]
  string <- cbind(camp, date.in, date.out)
}

GetMedical2019 <- function(x){
  trm01 <- x[4, ncol(x)]
  trm02 <- x[5, ncol(x)]
  trm03 <- x[6, ncol(x)]
  trm04 <- x[7, ncol(x)]
  trm05 <- x[8, ncol(x)]
  sum.t <- x[9, ncol(x)]
  ins.t <- x[10, ncol(x)]
  dys01 <- x[15, ncol(x)]
  dys02 <- x[16, ncol(x)]
  dys03 <- x[17, ncol(x)]
  dys04 <- x[18, ncol(x)]
  dys05 <- x[19, ncol(x)]
  dys06 <- x[20, ncol(x)]
  dys07 <- x[21, ncol(x)]
  dys08 <- x[22, ncol(x)]
  dys09 <- x[23, ncol(x)]
  dys10 <- x[24, ncol(x)]
  dys11 <- x[25, ncol(x)]
  dys12 <- x[26, ncol(x)]
  dys13 <- x[27, ncol(x)]
  sum.d <- x[28, ncol(x)]
  ins.d <- x[29, ncol(x)]

  string <- cbind(trm01, trm02, trm03, trm04, trm05, sum.t, ins.t,
    dys01, dys02, dys03, dys04, dys05, dys06, dys07, dys08, dys09, dys10, 
    dys11, dys12, dys13, sum.d, ins.d)
  return(string)
}

GetMedical2018 <- function(x){
  trm01 <- x[4, ncol(x)]
  trm02 <- x[5, ncol(x)]
  trm03 <- x[6, ncol(x)]
  trm04 <- x[7, ncol(x)]
  trm05 <- x[8, ncol(x)]
  trm06 <- x[9, ncol(x)]
  sum.t <- x[10, ncol(x)]
  ins.t <- x[11, ncol(x)]
  dys01 <- x[16, ncol(x)]
  dys02 <- x[17, ncol(x)]
  dys03 <- x[18, ncol(x)]
  dys04 <- x[19, ncol(x)]
  dys05 <- x[20, ncol(x)]
  dys06 <- x[21, ncol(x)]
  dys07 <- x[22, ncol(x)]
  dys08 <- x[23, ncol(x)]
  dys09 <- x[24, ncol(x)]
  dys10 <- x[25, ncol(x)]
  dys11 <- x[26, ncol(x)]
  dys12 <- x[27, ncol(x)]
  dys13 <- x[28, ncol(x)]
  sum.d <- x[29, ncol(x)]
  ins.d <- x[30, ncol(x)]

  string <- cbind(trm01, trm02, trm03, trm04, trm05, trm06, sum.t, ins.t,
    dys01, dys02, dys03, dys04, dys05, dys06, dys07, dys08, dys09, dys10, 
    dys11, dys12, dys13, sum.d, ins.d)
  return(string)
}