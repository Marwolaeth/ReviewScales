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

Sys.setenv(OLLAMA_TIMEOUT = 1200)
options(timeout = max(1200, getOption('timeout')))

## Данные для примеров ----
examples <- readxl::read_excel('data/example-sentences.xlsx')
# Umbrella, хотя и предлагает интересные решения, оказывается незамеченным среди более громких имен.
# Несмотря на то, что у Umbrella есть интересные решения, они находятся в тени более известных конкурентов.
# Umbrella все еще мало известны широкой публике, хотя у них есть достойные внимания разработки.

# Хотя Umbrella предлагает качественные продукты, его имя потерялось среди более громких брендов.
# Несмотря на то что Umbrella предлагает высококачественные продукты, его бренд затерялся на фоне более известных марок.
# Хотя Umbrella производит отличные товары, его имя не так заметно по сравнению с более громкими брендами.
# Несмотря на качество продукции Umbrella, его название не выделяется среди более популярных брендов.
# Хотя Umbrella предлагает товары высокого качества, его репутация затмевается более известными брендами.

# Некоторые наблюдатели полагают, что Umbrella может стать символом инноваций в своей области.

# Я всегда говорю друзьям: если хотите что-то особенное, смотрите в сторону Umbrella.

## Список моделей ----
load('data/models/models.RData')
# chat_models <- ollamar::list_models(output = 'df') |>
#   dplyr::mutate(model = dplyr::row_number()) |>
#   dplyr::select(name, model) |>
#   tibble::deframe()

chat_models <- seq_along(ellmer:::ollama_models()) |>
  purrr::set_names(ellmer:::ollama_models())

## Единый объект для оценки ----
universal_brand_name <- 'Y'

## Шкала тахометров ----
GAUGE_SCALE <- 5

## Prompts ----
### System ----
DEFAULT_SYSTEM_PROMPT_TEMPLATE_file <- file.path(
  'data',
  'prompts',
  'semdiff-template-system-prompt-1.1.md'
)
DEFAULT_SYSTEM_PROMPT_TEMPLATE <- readr::read_file(
  DEFAULT_SYSTEM_PROMPT_TEMPLATE_file
)

DEFAULT_SYSTEM_PROMPT_TEMPLATE <- stringr::str_replace(
  DEFAULT_SYSTEM_PROMPT_TEMPLATE,
  stringr::fixed('{{universal_brand_name}}'),
  stringr::fixed(universal_brand_name)
)

### User ----
DEFAULT_USER_PROMPT_TEMPLATE_file <- file.path(
  'data',
  'prompts',
  'semdiff-template-user-prompt.md'
)
DEFAULT_USER_PROMPT_TEMPLATE <- readr::read_file(
  DEFAULT_USER_PROMPT_TEMPLATE_file
)

DEFAULT_USER_PROMPT_TEMPLATE <- stringr::str_replace(
  DEFAULT_USER_PROMPT_TEMPLATE,
  stringr::fixed('{{universal_brand_name}}'),
  stringr::fixed(universal_brand_name)
)

### Placeholders ----
ALLOWED_PLACEHOLDERS_NLI <- c(
  'brand_name',
  'hypothesis'
)

ALLOWED_PLACEHOLDERS_CHAT <- c(
  'text',
  '{{text}}',
  'n_scales',
  'scale_names',
  'scaleset_description',
  'general_example',
  'scaleset_example'
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
