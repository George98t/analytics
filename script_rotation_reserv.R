'Распределение прихода со складов резерва'

options(scipen = 999) # исключаем свёртку больших чисел в экспоненциальный формат 

library(tibble)
library(dplyr)
library(tidyr)
library(data.table)



# загружаем продажи, приход, остатки, лимиты в потребности по предпринимателю 
vikladka_agg <- read.csv('V:\\Аналитический отдел\\R\\26_07\\prodazhi.csv', header = TRUE, sep = ";", quote = "\"",
                         dec = ".", fileEncoding = 'UTF-8')
prihod <- read.csv('V:\\Аналитический отдел\\R\\26_07\\prihod.csv', header = TRUE, sep = ";", quote = "\"",
                   dec = ".", fileEncoding = 'UTF-8')
ostatki <- read.csv('V:\\Аналитический отдел\\R\\26_07\\ostatki.csv', header = TRUE, sep = ";", quote = "\"",
                    dec = ".", fileEncoding = 'UTF-8')

limit_by_ip <- read.csv('V:\\Аналитический отдел\\R\\26_07\\prodazhi_ip.csv', header = TRUE, sep = ";", quote = "\"",
                        dec = ".", fileEncoding = 'UTF-8')


# сохраняем исходные версии лимитов по предпринимателю  и продаж 
lbi <- limit_by_ip
va <- vikladka_agg

# заменяем имя колонки количество
names(prihod)[names(prihod) == 'kolichestvo'] <- 'count'


# добавляем региональную принадлежность в остатках и продажах 
ostatki['reg_ident'] <- ''

ostatki['reg_ident'] <- ifelse(ostatki$predprinimatel == 'ИП Кузнецов Добрыня Андреевич' | ostatki$predprinimatel == 'ИП Гадальшина Клара Казбековна' |
                                 ostatki$predprinimatel == 'ИП Лясова Евгения Вячеславовна',
                               'mosc', 'uf')
vikladka_agg['reg_ident'] <- ''

vikladka_agg['reg_ident'] <- ifelse(vikladka_agg$predprinimatel == 'ИП Кузнецов Добрыня Андреевич' | vikladka_agg$predprinimatel == 'ИП Гадальшина Клара Казбековна' |
                                      vikladka_agg$predprinimatel == 'ИП Лясова Евгения Вячеславовна',
                                    'mosc', 'uf')


# Добавляем поля, которые будут заполнять в процессе работы кода 
prihod['date_rot'] <- as.Date('')
prihod['last_action'] <-''
prihod['poluchatel'] <-''

# статус будет устанавливаться в процессе работы скрипта 
vikladka_agg['status'] <- ''

# создаем дополнительную таблицу остатков для подсчёта дублей номенклатуры на точке 
ost <- ostatki %>% group_by(nomenklatura, podrazdelenye_kust) %>% summarise(freq = sum(kolichestvo))


# получение наименований строк остатков
rns <- rownames(prihod)


# код распределяет товар со складов, сначала внутри своего предпринимателя, затем внутри региона, затем по всем остальным 
# при этом отслеживается частота номенклатуры в точках (freq), учитывается потребность по предпринимателю (rasp_potr_without_rot) из таблицы limit_by_ip
# при распределении по точкам учитывается коэффициент средних продаж к остаткам, который в процессе пересчитывается с присвоением ему новых товаров 
# учитываются общие срднемесячные продажи точки avg_month_sales_at_all



# итерация по остаткам-излишкам
for (i in 1:length(rns)) {
  
  # запоминаем номенклатуру, предпринимателя, реквизиты 
  nomenklatura <- prihod[i,"nomenklatura"]
  predpr <- prihod [i, 'predprinimatel']
  rekv <-  prihod [i, 'rekvizity']
  
  # запоминаем региональность, наименование строки
  rg <- ostatki[i, 'reg_ident']
  rn <- rns[i]
  
  print(rn)
  
  rows <- subset(vikladka_agg, 
                 rekvizity == prihod[rn, "rekvizity"]
                 & status != 'break'
                 & prihod[rn,'count'] > 0,
                 select = c(podrazdelenye_kust, koef_sr_prod_ost, reg_ident, predprinimatel, avg_month_sales_at_all))
  
  print(paste('реквизит:', prihod[i, "rekvizity"]))
  
  # если по предпринимателю резерва по реквизиту еще есть потребность - распределяем своему ИП
  if (isTRUE(limit_by_ip[limit_by_ip$rekvizity == prihod[rn, "rekvizity"] & 
                         limit_by_ip$predprinimatel == predpr, 'rasp_potr_without_rot' ] > 0)
      & length(rownames(rows[rows$predprinimatel == predpr, ])) >0 
      & length(rownames(rows) > 0)
  ) 
  { print("Выполнилось условие 1")
    #первый цикл подтягивает частотность номенклатуры в остатках к rows
    rows_2 <- rows[rows$predprinimatel == predpr, ]
    rows_2['freq'] <- as.numeric('')
    rnss <- rownames(rows_2)
    
    for (x in 1:length(rnss)){
      pod_kust <- rows_2[x,"podrazdelenye_kust"]
      
      if (length(rownames(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, ])) == 0 |
          length(rownames(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, ])) > 1){
        rows_2[x, "freq"]<- 0
      }
      else {rows_2[x, "freq"] <-as.numeric(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, "freq"])
      }
    }
    # print(paste('есть потребность в своём регионе', rows))
    # сортировка по частоте номенклатуры в точке, коэффициенту средних продаж к остаткам, общих продаж точки 
    sorted <-  rows_2[with(rows_2, order(freq, -koef_sr_prod_ost, -avg_month_sales_at_all )),]
    rns2 <- rownames(sorted) # присваиваем отсортированный фрейм 
  }
  # Если потребности по своему ИП нет: распределяем по остальным
  else if (length(rownames(rows)) > 0 
           & isTRUE(limit_by_ip[limit_by_ip$rekvizity == rekv &
                                limit_by_ip$predprinimatel == predpr, 'rasp_potr_without_rot' ] >= -1) |
           length(rownames(rows)) > 0
           & length(rownames(limit_by_ip[limit_by_ip$rekvizity == rekv &
                                         limit_by_ip$predprinimatel == predpr,  ])) == 0
  ) 
  { print("Выполнилось условие 2")
    if (length(rownames(rows)) <= 2) # страховочное минимальное количество строк 
    {rows_2 <-rows 
    }
    else { # если в полученном фрейме не остается строк когда мы исключаем предпринимателя, которые относится к резерву: переходим к следующей итерации цикла
            if (length(rownames(rows[rows$predprinimatel != predpr, ])) == 0 ){next}
            else  rows_2 <- rows[rows$predprinimatel != predpr, ] # если есть присваиваем 
    }
    #первый цикл подтягивает частотность номенклатуры в остатках к rows
    rows_2['freq'] <- as.numeric('')
    rnss <- rownames(rows_2)
    for (x in 1:length(rnss)){
      pod_kust <- rows_2[x,"podrazdelenye_kust"]
      if (length(rownames(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, ])) >= -1 |
          length(rownames(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, ])) > 1){
        rows_2[x, "freq"]<- 0
      }
      else {rows_2[x, "freq"] <-as.numeric(ost[ost$podrazdelenye_kust == pod_kust & ost$nomenklatura == nomenklatura, "freq"])
      }
    }
    # Определяем к какому региону относится склад, сортируем данные по этому региону 
    if(rg =='mosc') {
      # сортировка по потребности, по убыванию
      sorted <-  rows_2[with(rows_2, order(reg_ident, freq, -koef_sr_prod_ost, -avg_month_sales_at_all )),]
      rns2 <- rownames(sorted)
    }
    else if(rg =='uf'){
      sorted <- rows_2[with(rows_2, order(rev(reg_ident), freq, -koef_sr_prod_ost, -avg_month_sales_at_all )),] 
      rns2 <- rownames(sorted)
    }
  }
  else if (length(rownames(rows)) == 0) { #обрабатываем если строк не нашлось 
    print("Выполнилось условие 3")
    next
  }
  
  for(i in 1:length(rns2)) { # перебираем отсортированные строки
    
    predprinimatel <- sorted[i,"predprinimatel"] 
    pomechenie <- sorted[i,"podrazdelenye_kust"] #помещение магазина с высокой потребностью
    
    # если не было ранее остатков по этой номенклатуре, соответствущую строку в фрейме остатков  
    if (length(rownames(ost[ost$podrazdelenye_kust == pomechenie & ost$nomenklatura == nomenklatura, ])) == 0){
      d <- data.frame(pomechenie, nomenklatura, 1)
      names(d) <- c("podrazdelenye_kust", "nomenklatura", "freq")
      ost <- rbind(ost, d)
    }
    # если были: присваем +1  
    else {ost[ost$podrazdelenye_kust == pomechenie & ost$nomenklatura == nomenklatura, "freq"] <-
      ost[ost$podrazdelenye_kust == pomechenie & ost$nomenklatura == nomenklatura, "freq"]+1 }
    
    # уменьшаем потребность предпринимателю, которому присвоили 
    limit_by_ip[limit_by_ip$rekvizity == rekv &
                  limit_by_ip$predprinimatel == predprinimatel, 'rasp_potr_without_rot' ] <- limit_by_ip[limit_by_ip$rekvizity == rekv &
                                                                                                           limit_by_ip$predprinimatel == predprinimatel, 'rasp_potr_without_rot' ]-1
    
    # если потребность становится = -1: присваиваем статус break, чтобы строки продаж по точкам этого ИП не могли стать получателями на этот реквизит
    if ( length(rownames(limit_by_ip[limit_by_ip$rekvizity == rekv & limit_by_ip$predprinimatel == predprinimatel,  ])) > 0 & 
         isTRUE(limit_by_ip[limit_by_ip$rekvizity == rekv & limit_by_ip$predprinimatel == predprinimatel, 'rasp_potr_without_rot' ]<= -1 ))
    {
      vikladka_agg[vikladka_agg$rekvizity == rekv & vikladka_agg$predprinimatel == predprinimatel, 'status' ] <- 'break'
    }
    # Проставляем получателя, дату, действие, увеличиваем количество остатков в продажах и пересчитываем коэффициент средних продаж к остаткам
    prihod[rn,]$poluchatel <- pomechenie #получает одна строка в остатках
    prihod[rn,]$date_rot <- Sys.Date()
    prihod[rn,]$last_action <- 'distribution'
    row_index <- rns2[i]
    # добавляю остатки в продажи пересчитываю коэф
    vikladka_agg[row_index,]$ostatki <- vikladka_agg[row_index,]$ostatki +1
    vikladka_agg[row_index,]$koef_sr_prod_ost <- vikladka_agg[row_index,]$avg_month_sales/vikladka_agg[row_index,]$ostatki
    break
  }
}



# экспортируем данные 
library(writexl)

write_xlsx(vikladka_agg, "V:\\Аналитический отдел\\R\\Exported\\prodazhi_rotation_reserv_26_07_3.xlsx")
write_xlsx(prihod, "V:\\Аналитический отдел\\R\\Exported\\prihod_rotation_reserv_26_07_3.xlsx")
write_xlsx(limit_by_ip,  "V:\\Аналитический отдел\\R\\Exported\\limit_by_ip_rotation_reserv_26_07_3.xlsx" )
write_xlsx(lbi,  "V:\\Аналитический отдел\\R\\Exported\\lbi_rotation_reserv_26_07_3.xlsx" )


