setwd("~/Documents/work/cert_forms/")

library(openxlsx)
library(dplyr)
library(stringr)
library(lubridate)

ExtractData <- function(x) {
  x <- read.xlsx(x)
  money <- str_replace_all(string = x[8, 3], pattern=" ", repl="")
  money <- str_replace_all(string = money, pattern=",", repl=".")
  money <- as.numeric(money)
  
  contract <- str_replace_all(string = x[4, 1], pattern="[а-я]|[А-Я]|[:]", repl="")
  contract <- strsplit(contract, split = ". № ")
  contract_date <- contract[[1]][1]
  contract_date <- str_replace_all(contract_date, pattern = " ", replacement = "")
  # contract_date <- dmy(contract_date)
  contract_no <- contract[[1]][2]
  contract_no <- str_replace_all(contract_no, pattern = " ", replacement = "")
  
  camper <- strsplit(x[5, 1], ", ", "")
  
  name <- camper[[1]][1]
  name <- str_replace(name, "На ", "")
  
  birthdate <- camper[[1]][2]
  birthdate <- str_replace(birthdate, " г.р.", "")
  # birthdate <- dmy(birthdate)
  
  string <- cbind(contract_no, contract_date, name, birthdate, money)
  return(string)
}

file.names <- list.files(path = "./", recursive = TRUE, pattern = "*.xlsx")
cert.data <- lapply(file.names, ExtractData)
cert.data <- data.frame(matrix(unlist(cert.data), nrow = length(cert.data), 
                               byrow = TRUE))

names(cert.data) <- c("contract_no", "contract_date", "camper_name",
                      "camper_birthdate", "price")
cert.data$contract_date <- as.Date(cert.data$contract_date, format = "%d.%m.%Y")
cert.data$camper_birthdate <- as.Date(cert.data$camper_birthdate, format = "%d.%m.%Y")
cert.data$contract_no <- as.character(cert.data$contract_no)
cert.data$camper_name <- as.character(cert.data$camper_name)
cert.data$price <- as.numeric(as.character(cert.data$price))

cert.data <- filter(cert.data, contract_date < "2020-04-01")

pay.off <- (cert.data %>%
  filter(price <= 30000) %>%
  summarise(sum = sum(price)) +
  cert.data %>%
  filter(price > 30000) %>%
  summarise(sum = 30000 * n())) / 1000
pay.off
