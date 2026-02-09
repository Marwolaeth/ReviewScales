# GLOBAL ----
library(shiny)
library(shinyjs)
library(shinyvalidate)
library(shinycssloaders)
library(shinyhelper)
library(readxl)
library(readr)
library(glue)
library(stringr)
library(tictoc)
library(reactable)

options(ellmer_timeout_s = 120000000)

## Данные для примеров ----
examples <- readxl::read_excel('data/example-sentences.xlsx')
# Очень мощный пылесос. Мешок объемный, долго не требует замены. Восхитительная вторая насадка для труднодоступных мест (не требуется снимать, она просто съезжает вниз по трубе). Пользуюсь пол года примерно. ламинат на минимальной мощности с поднятой щеткой надо пылесосить, иначе не оторвать щетку от пола.
# 
# Достоинства: Мощный пылесос
# Недостатки: Пока не выявлено
# Комментарий: Искали более менее бюджетный мощный пылесос с мешком. Ожидания оправдались. Рекомендую к покупке.
# 
# Достоинства: небольшой, мощный, есть регулировка мощности и самособирающийся шнур
# Недостатки: маленькие красные штуки типа щеток, на насадках, отлетели на второй раз, но не страшно
#
# Хороший недорогой пылесос,  соски отлично

# Пылесос сделан в Китае ,а не во Франции как многие пишут. Мощность всасывания и правда очень сильная очень сложно пылесосить.У меня есть Самсунг с 480вт всасывания так вот Тефаль на порядок мощнее наверное все 550 или 600 . Не хватает регулировки мощности двигателя. Приходится открывать клапан на ручке что бы оторвать от ковра или пола,что отменяет всю фильтрацию в 99 % через клапан также летит часть пыли , также не герметичное соединение шланга с телескопической трубкой оттуда тоже выдувается часть пыли. Конструкция циклона очень неудобная для быстрого очищения контейнера, особенно если в доме есть длинноволосые . лучше очищать над ванной т.к часть собранного мусора при разборке всегда высыпается. По громкости он тише реально тише многих моделей пылесосов. Шнур тоньше привычного круглого провода .Выдув воздуха идёт вперёд из-за контейнера,что очень не привычно. Герметичность контейнера так себе. Это моё субъективное мнение о пылесосе. Судя по отзывам большинство в восторге от него. Я бы выбрал другой пылесос с регулировкой мощности двигателя и другой конструкцией циклонного контейнера с более удобной очисткой.
# Недостатки: Всё перечисленное выше. Может это придирки, но всё-таки удобство пользования это не маловажно.
# Комментарий: За эту цену конкурентов по мощности наверное нет среди других брендов и моделей. Он значительно тише. Своё мнение я описал выше.

## Список моделей ----
load('data/models/models.RData')
# chat_models <- ollamar::list_models(output = 'df') |>
#   dplyr::mutate(model = dplyr::row_number()) |>
#   dplyr::select(name, model) |>
#   tibble::deframe()

ollama_model_names <- ellmer:::models_ollama()$id
chat_models <- seq_along(ollama_model_names) |>
  purrr::set_names(ollama_model_names)

## Шкала тахометров ----
GAUGE_SCALE <- 2

## Prompts ----
### System ----
DEFAULT_SYSTEM_PROMPT_TEMPLATE_file <- file.path(
  'data',
  'prompts',
  'reviews-template-system-prompt-1.0.md'
)
DEFAULT_SYSTEM_PROMPT_TEMPLATE <- readr::read_file(
  DEFAULT_SYSTEM_PROMPT_TEMPLATE_file
)

### User ----
DEFAULT_USER_PROMPT_TEMPLATE_file <- file.path(
  'data',
  'prompts',
  'reviews-template-user-prompt.md'
)
DEFAULT_USER_PROMPT_TEMPLATE <- readr::read_file(
  DEFAULT_USER_PROMPT_TEMPLATE_file
)

### Placeholders ----
ALLOWED_PLACEHOLDERS_NLI <- c(
  'product_type',
  'hypothesis'
)

ALLOWED_PLACEHOLDERS_CHAT <- c(
  'text',
  '{text}',
  'n_scales',
  'scale_names',
  'scaleset_description',
  'product_type'
)

## Отображение Reactable ----
options(reactable.language = reactableLang(
  searchPlaceholder = 'Поиск…',
  noData = 'Нет записей',
  pageInfo = 'Записи {rowStart}–{rowEnd} из {rows}',
  pagePrevious = '\u276e',
  pageNext = '\u276f',
  
  # Accessible labels for assistive technologies such as screen readers.
  # These are already set by default, but don't forget to update them when
  # changing visible text.
  pagePreviousLabel = 'Предыдущая страница',
  pageNextLabel = 'Следующая страница',
  pageSizeOptions = 'Показывать {rows} записей'
))

column_definitions <- list(
  text = colDef(name = 'Текст', resizable = TRUE),
  scale = colDef(name = 'Шкала', filterable = TRUE),
  items = colDef(name = 'Пункты'),
  .score = colDef(
    name = 'Значение',
    format = colFormat(digits = 3, locales = 'ru-UA')
  ),
  model = colDef(name = 'Модель', filterable = TRUE),
  comment = colDef(name = 'Комментарий', filterable = TRUE)
)
