# Предварительно загрузить аналитические отчёты о недозаездах
# и аналитический отчёт об отдыхе воспитанников ДТСЗН (см. README)
# из АИС "Мосгортур"

library(openxlsx)
camps <- read.xlsx("~/data/camps.xlsx")

fam2020 <- read.xlsx("~/aism/2020/nonarrivals_fam.xlsx")
fam2020 <- fam2020[5:nrow(fam2020), 1]
fam2020 <- unique(fam2020)

ind2020 <- read.xlsx("~/aism/2020/nonarrivals_ind.xlsx")
ind2020 <- ind2020[5:nrow(ind2020), 1]
ind2020 <- unique(ind2020)

dep2020 <- read.xlsx("~/aism/2020/nonarrivals_dep.xlsx")
dep2020 <- dep2020[5:nrow(dep2020), 4]
dep2020 <- unique(dep2020)

orp2020 <- read.xlsx("~/aism/2020/nonarrivals_orp.xlsx")
orp2020 <- orp2020[5:nrow(orp2020), 1]
orp2020 <- unique(orp2020)

camps2020 <- c(ind2020, fam2020, orp2020, dep2020)
camps.new <- setdiff(camps2020, camps$camp_name)

camps[c(nrow(camps) + 1: length(camps.new)), ]$camp_name <- camps.new

write.xlsx(camps, "~/data/camps_new.xlsx")

# После сохранения дополнить информацию о лагерях вручную и
# сохранить как camps.xlsx
