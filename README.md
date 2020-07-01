# Скрипты для обработки данных из АИС "Отдых" и АИС "Мосгортур"

## Размещение файлов

Перед использованием скриптов рекомендуется правильно разместить
файлы в системе (в противном случае нужно вручную указать правильные
пути до файлов в скриптах вручную).

Для этого _перед началом работы_ нужно открыть RStudio и в консоли
(нижнее правое окно) выполнить команду `path.expand("~/")`.
Результатом выполнения команды будет _путь до домашней папки_ в вашей
системе. В компьютерах под управлением Windows с большой вероятностью
это будет папка "Документы" (`C/Users/username/Documents/`), но,
возможно, и домашняя папка пользователя(`C/Users/username/`). В MacOS
и Linux это будет домашняя папка пользователя (`/Users/username/` и
`/home/username/` соответственно).

В этой папке нужно разместить файлы следующим образом:

- весь **код** (то есть все скрипты) разместить в папке `git`;
- все выгрузки из АИС "Мосгортур" разместить в папке `aism`;
- все выгрузки из АИС "Отдых" разместить в папке `aiso`.
- создать папку `data` (в неё будут сохраняться результаты выполнения
  скриптов).

## Работа со скриптом [`aiso_register`](https://github.com/uasmgt/ais/tree/master/aiso)

Скрипт позволяет объединить данные, экспортированные из реестра
заявлений или реестра отдыхающих из АИС "Отдых".

### Загрузка данных из АИС "Отдых"

АИС "Отдых" не позволяет выгружать большие массивы (примерно более 35
тысяч строк) за раз (после обработки запроса система "выкидывает"
пользователя из системы).

Для сохранения Реестра заявлений я предлагаю экспортировать данные поэтапно при помощи фильтров "Цель обращения" и "Вид принятого решения": 

1. Цель обращения -> Детский отдых / Статус заявления -> --Не
   выбрано--
2. Цель обращения -> Компенсация за путёвку / Статус заявления ->
   --Не выбрано--
3. Цель обращения -> Молодёжный отдых / Статус заявления -> --Не
   выбрано--
4. Цель обращения -> Сертификат на отдых и оздоровление, 3-7 лет /
   Статус заявления -> --Не выбрано--
5. Цель обращения -> Сертификат на отдых и оздоровление, 7-15 лет /
   Статус заявления -> --Не выбрано--
6. Цель обращения -> Совместный отдых / Статус заявления -> Отказ в
   регистрации
7. Цель обращения -> Совместный отдых / Статус заявления -> Отозвано.
   По инициативе заявителя
8. Цель обращения -> Совместный отдых / Статус заявления -> Отказ в
   предоставлении услуги
9. Цель обращения -> Совместный отдых / Статус заявления -> Услуга
   оказана

Путь до папки, в которую экспортируются файлы, не имеет значения. Для
систематизации я использую следующую схему `aiso/год/дата_выгрузки/`
(например `aiso\2020\20200629\`).

После сохранения файлов необходимо извлечь файлы из zip-архивов. Если
файлы извлекаются в подпапки, то переносить из них файлы не нужно.
Переименовывать файлы не нужно. Удалять архивы необязательно.

**Важно, чтобы в папке не было посторонних xlsx-файлов.**

#### Разница между реестром заявлений и реестром отдыхающих

В 2016--2019 годах заявители могли поменять решение по виду
оказываемой услуги, то есть подать заявление на получение путёвки, а
на втором этапе выбрать получение сертификата на отдых и оздоровление
(и наоборот).

В выгрузках из реестра заявлений АИС "Отдых" эта смена не
отображается. Поэтому для получения сводного массива получателей
путёвок/сертификатов за эти годы необходимо выбрать вкладку "Реестр
отдыхающих" и установить фильтр "Вид принятого решения -> Субсидия
(для сертификатов) или Путёвка (для путёвок)".

В 2020 году заявители не могут менять свой изначальный выбор, поэтому
в этом случае можно пользоваться сформированным массивом заявлений.

### Использование скрипта

1. В RStudio открываем скрипт `aiso_register`.
2. В 17-й строке раскомментируем путь до сохранённых и извлечённых
   файлов (для этого нужно удалить символ `#` в начале строки). Например, `setwd("~/aiso/2020/20200629/")`.
3. В 73-й и 76-й строках задаём название для итогового задаём место,
   куда будет сохранён массив.
4. Сохраняем изменения.
5. Исполняем скрипт, это можно сделать двумя способами:
   - выделить весь код в редакторе кода и нажать Ctrl + Enter;
   - в консоли ввести `source(~/git/aiso/aiso_register.R, encoding =
     "UTF-8")` и нажать Enter.

В дальнейшем при работе с массивом в R необходимо загрузить массив:
например, `load("~/data/aiso2020_200629.rda")`. Для работы в Excel 
нужно открыть сохранённый csv-файл. Названия столбцов можно подсмотреть в скрипте [`aiso_register`](https://github.com/uasmgt/ais/blob/master/aiso/aiso_labels.R).

### Дальнейший анализ

Примеры простейшего анализа с использованием библиотек `tidyr` и
`dplyr` приведены в файле [`analysis_example`](https://github.com/uasmgt/ais/blob/master/aiso/analysis_example.R).

## Работа со скриптами [`aism`](https://github.com/uasmgt/ais/tree/master/aism)

Скрипты в этой папке создают сводные массивы с данными о недозаездах
отдыхающих в лагеря и санатории и обращениях отдыхающих к
медицинскому персоналу.

### Загрузка данных из АИС "Мосгортур"

Для формирования массива необходимо выгрузить данные о состоявшихся заездах из АИС "Мосгортур".

Данные нужно сохранять в папку `aism` в подпапку для соответствующего
года.

Для анализа кампаний 2016--2019 годов я сохранял данные о заездах для
индивидуального отдыха и индивидуального отдыха детей-сирот в
подпапку `arrivals_ind`, для совместного отдыха и совместного отдыха
детей-сирот --- в папку `arrivals_fam`. В 2020 году я предлагаю
сохранять данные о заездах детей-сирот в отдельные подпапки
(`arrivals_dep` для индивидуального и `arrivals_orp` для
совместного).

В АИС "Мосгортур" вносятся данные обо _всех_ заездах и турах, организуемых ГАУК "МОСГОРТУР". Не все из них относятся к оздоровительным кампаниям. Чтобы не включать в анализ исключительно коммерческие заезды и туры, я предлагаю ориентироваться на перечни заездов в аналитических отчётах АИС "Мосгортур":

- [для индивидуального отдыха](https://ais.mosgortur.ru/AnalyticReport?ReportType=0A587355-178F-4BFD-9CE8-79708E39D808);
- [для совместного отдыха](https://ais.mosgortur.ru/AnalyticReport?ReportType=2E151C59-105D-47B0-98A2-0C45A08BFD8C);
- [для индивидуального отдыха детей-сирот](https://ais.mosgortur.ru/AnalyticReport?ReportType=84A4C70B-C322-4586-8D14-CE9BD7F7B798);
- [для совместного отдыха детей-сирот](https://ais.mosgortur.ru/AnalyticReport?ReportType=D90C7591-8DAA-4078-96E7-21114A32A2FB).

Сверка с этими перечнями позволит, с одной стороны, не упустить ни 
одного заезда, с другой --- не включить в массив лишние заезды.

### Обновления списка лагерей

В каждой кампании добавляются новые лагеря и санатории. Чтобы скрипт
правильно "подтягивал" регионы, в которых располагаются лагеря,
необходимо обновлять их список.

Этому служит скрипт [`new_camps`](https://github.com/uasmgt/ais/blob/master/new_camps.R).

Для его выполнения нужно сохранить в подпапку, соответствующую году
кампании в папке `aism`,  аналитические отчёты о недозаездах,
перечисленные выше.

Скрипт проверит, каких лагерей нет в файле `camps.xlsx` в папке
`data`, добавит их названия в столбец `camp_name`, и сохранит список
лагерей в файл `camps_new.xlsx`.

Затем этот файл нужно будет дополнить данными о лагерях и санаториях.
Для этого нужно скопировать ячейку в столбце `camp_name` (**важно НЕ
МЕНЯТЬ содержимое этой ячейки!**) и найти информацию об этом
лагере/санатории в [АИС "Мосгортур"](https://ais.mosgortur.ru/Hotels/Search).

Затем обновлённый список нужно сохранить под названием `camps.xlsx`
(то есть перезаписать старый файл).

### Информация о недопусках

В течение оздоровительной кампании могут появляться служебные записки
о недопусках по результатам медицинских осмотров. ФИО недопущенных
детей необходимо внести в файл `denials_ind2020` (и последующие
годы). Даже если недопусков не будет, то для корректного выполнения
скрипта нужно создать соответствующий "пустой" файл (например,
скопировать под соответствующим названием файл `denials_ind2018`)

### Выполнение скрипта

После сохранения данных о заездах в соответствующие папки
необходимо выполнить нужный скрипт:

- `dep2020` --- для индивидуального отдыха детей-сирот;
- `fam2020` --- для совместного отдыха;
- `ind2020` --- для индивидуального отдыха;
- `orp2020` --- для совместного отдыха детей-сирот.

Скрипт можно выполнить как выделив код в редакторе кода и нажав Ctrl
+ Enter, так и исполнив в консоли команду
`source("~/git/ais/aism/ind2020.R", encoding = "UTF-8")` (`ind2020`
нужно заменить на необходимый скрипт).

После выполнения скрипта массивы будут в папке `data` под названием
`data_ind/fam/dep/orf2020` в форматах rda и csv.

Подписи к колонкам можно подсмотреть в скриптах
[`labels_ind`](https://github.com/uasmgt/ais/blob/master/aism/labels_ind.R)
и [`labels_fam`](https://github.com/uasmgt/ais/blob/master/aism/labels_fam.R).
