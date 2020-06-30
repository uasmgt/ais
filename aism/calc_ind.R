# Подсчёт доли недозаездов
dataset$non_vouchers <- dataset$nonarrivals_vouchers / 
  (dataset$fact_vouchers + dataset$nonarrivals_vouchers)
dataset$non_department <- dataset$nonarrivals_dep /
  (dataset$nonarrivals_dep + dataset$fact_dep)

# Доли детей разных категорий от числа детей ---------------------------
dataset$per_department      <- dataset$fact_dep        / dataset$fact_total
dataset$per_disabled        <- dataset$disabled        / dataset$fact_total
dataset$per_disorders       <- dataset$disorders       / dataset$fact_total
dataset$per_mental          <- dataset$mental          / dataset$fact_total
dataset$per_muscle_skeleton <- dataset$muscle_skeleton / dataset$fact_total
dataset$per_dysfunction     <- dataset$dysfunction     / dataset$fact_total
dataset$per_sensorial       <- dataset$sensorial       / dataset$fact_total

# Количество обращений на одного отдыхающего
dataset$dys_per_men         <- dataset$dys_sum / dataset$fact_total
dataset$trm_per_men         <- dataset$trm_sum / dataset$fact_total

# Удалить заезды без отдыхающих ----------------------------------------
dataset <- dataset %>% filter(fact_total + nonarrivals_total != 0)

