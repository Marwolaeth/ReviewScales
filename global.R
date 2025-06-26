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

options(ellmer_timeout_s = 1200)

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
