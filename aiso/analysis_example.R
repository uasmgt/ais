# Загрузка массива -----------------------------------------------------
load("~/data/aiso2020_200629.rda") # указать корректный путь до файла

# Библиотеки -----------------------------------------------------------
library(tidyr)
library(dplyr)

# Примеры расчётов  ----------------------------------------------------
# Количество заявлений по цели обращения
aiso.register %>% 
  group_by(purpose) %>% 
  summarise(n = n())

# Количество удовлетворённых заявлений по цели обращения
aiso.register %>% 
  filter(status == "Услуга оказана") %>% # фильтрует заявления по статусу
  group_by(purpose) %>%                  # группирует заявления по цели обращения
  summarise(num = n())                   # подсчитывает количество

# Количество удовлетворённых заявлений по цели обращения (c процентами)
aiso.register %>% 
  filter(status == "Услуга оказана") %>% 
  group_by(purpose) %>%
  summarise(num = n()) %>%
  mutate(per = num / sum(num) * 100) # подсчёт процентов

# Количество удовлетворённых заявлений по укрупнённым целям обращения (с процентами)
aiso.register %>% 
  filter(status == "Услуга оказана") %>% 
  mutate(purpose_short = case_when(grepl(purpose, pattern = "Детский лагерь") ~ "Индивидуальный отдых",
                                   grepl(purpose, pattern = "Совместный отдых") ~ "Совместный отдых",
                                   grepl(purpose, pattern = "Сертификат") ~ "Сертификат",
                                   grepl(purpose, pattern = "Молодёжный") ~ "Молодежный отдых",
                                   grepl(purpose, pattern = "Компенсация") ~ "Компенсация")) %>% 
  group_by(purpose_short) %>% 
  summarise(num = n()) %>%
  mutate(per = num / sum(num) * 100)

# Подсчёт по цели обращения в разрезе ребёнок / сопровождающий
aiso.register %>% 
  filter(status == "Услуга оказана") %>% 
  mutate(purpose_short = case_when(grepl(purpose, pattern = "Детский лагерь") ~ "Индивидуальный отдых",
                                   grepl(purpose, pattern = "Совместный отдых") ~ "Совместный отдых",
                                   grepl(purpose, pattern = "Сертификат") ~ "Сертификат",
                                   grepl(purpose, pattern = "Молодёжный") ~ "Молодежный отдых",
                                   grepl(purpose, pattern = "Компенсация") ~ "Компенсация"),
         age_group = case_when(age_camper <= 17 ~ "Ребёнок",
                               age_camper > 17 ~ "Сопровождающий")) %>% 
  group_by(age_group, purpose_short) %>% 
  summarise(num = n()) %>%
  spread(key = age_group, value = num)

# Сохранение результатов -----------------------------------------------
# преобразование в таблицу и запись в переменную
table <- aiso.register %>% 
  filter(status == "Услуга оказана") %>% 
  mutate(purpose_short = case_when(grepl(purpose, pattern = "Детский лагерь") ~ "Индивидуальный отдых",
                                   grepl(purpose, pattern = "Совместный отдых") ~ "Совместный отдых",
                                   grepl(purpose, pattern = "Сертификат") ~ "Сертификат",
                                   grepl(purpose, pattern = "Молодёжный") ~ "Молодежный отдых",
                                   grepl(purpose, pattern = "Компенсация") ~ "Компенсация"),
         age_group = case_when(age_camper <= 17 ~ "Ребёнок",
                               age_camper > 17 ~ "Сопровождающий")) %>% 
  group_by(age_group, purpose_short) %>% 
  summarise(num = n()) %>%
  spread(key = age_group, value = num) %>% 
  as.data.frame()
# сохранение в csv-файл
write.csv(table, "~/data/table_purpose.csv")
