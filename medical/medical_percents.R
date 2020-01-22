# Подгрузить данные о количестве отдыхающих
medical.data$kids <- data.arrivals$fact_total
medical.data$department <- data.arrivals$fact_dep
medical.data$disabled <- data.arrivals$disabled
medical.data$disorders <- data.arrivals$disorders_total
medical.data$mental <-  data.arrivals$mental
medical.data$muscle_skeleton <- data.arrivals$muscle_skeleton
medical.data$dysfunction <- data.arrivals$dysfunction
medical.data$sensorial <- data.arrivals$sensorial

# Удалить заезды без отдыхающих ----------------------------------------
medical.data <- medical.data %>% filter(kids != 0)

# Рассчитать доли детей разных категорий от числа детей ----------------
medical.data$per_department <- medical.data$department / medical.data$kids
medical.data$per_disabled <- medical.data$disabled / medical.data$kids
medical.data$per_disorders <- medical.data$disorders / medical.data$kids
medical.data$per_mental <- medical.data$mental / medical.data$kids
medical.data$per_muscle_skeleton <- medical.data$muscle_skeleton /medical.data$kids
medical.data$per_dysfunction <- medical.data$dysfunction / medical.data$kids
medical.data$per_sensorial <- medical.data$sensorial / medical.data$kids

medical.data$dys_per_men <- medical.data$dys_sum / medical.data$kids
medical.data$trm_per_men <- medical.data$trm_sum / medical.data$kids