# Скрипты для обработки данных из АИС "Отдых" и АИС "Мосгортур"

## Размещение файлов

Перед использованием скриптов рекомендуется правильно разместить файлы в системе[^files].

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


[^files]: В противном случае нужно вручную указать правильные пути до
          файлов в скриптах вручную.