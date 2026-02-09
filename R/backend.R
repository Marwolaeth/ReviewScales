library(tidyr)
library(text)
library(ellmer)

## Text Utils ----
reticulate::source_python(
  system.file(
    'python',
    'huggingface_Interface3.py',
    package = 'text',
    mustWork = TRUE
  )
)

## Back-end Functions ----
# Функция для семантического дифференциала с нулевым обучением
#' Семантический дифференциал с нулевым обучением
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. Она позволяет оценить текстовые данные по 
#' различным критериям, таким как инновационность или популярность.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param candidate_labels Вектор строк, содержащий названия полярностей для оценки.
#' @param template Шаблон гипотезы, который будет использоваться для классификации.
#' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
#'                к текстам перед классификацией. По умолчанию FALSE.
#' @param aggregation Метод агрегации для объединения оценок (можно выбрать 'max' или 'mean').
#' @param mask Вектор, определяющий влияние каждой полярности на финальный результат.
#'              По умолчанию c(-1, 1).
#' @param multi_label Логическое значение, указывающее, поддерживает ли модель
#'                    многоклассовую классификацию. По умолчанию FALSE.
#'
#' @return Возвращает числовое значение, представляющее собой итоговый балл
#'         для текста, рассчитанный на основе оценок по полярностям и маске.
#'         Результат можно интерпретировать как обобщенную оценку текста:
#'         - Положительное значение указывает на более высокую оценку по
#'           положительным полярностям.
#'         - Отрицательное значение указывает на более высокую оценку по
#'           отрицательным полярностям.
#'         - Значение близкое к нулю может указывать на сбалансированное
#'           восприятие текста по рассматриваемым полярностям.
#'
#' @examples
#' # Пример использования функции
#' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
#' model <- "model_name"
#' candidate_labels <- c("инновационный", "устаревший")
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot(
#'   texts = texts,
#'   model = model,
#'   candidate_labels = candidate_labels,
#'   template = template,
#'   mask = c(1, -1),
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot <- function(
    texts,
    model,
    candidate_labels,
    template,
    prefix = FALSE,
    aggregation = c('max', 'mean'),
    mask = c(-1, 1),
    multi_label = FALSE,
    append_neutral = FALSE,
    seed = 111,
    device = 'cpu'
) {
  aggregation <- match.arg(aggregation, c('max', 'mean'))
  aggregation <- match.fun(aggregation)
  
  if (prefix) texts <- paste('classification:', texts)
  
  if (append_neutral) {
    candidate_labels <- c(candidate_labels, 'ничего из перечисленного')
    mask <- c(mask, 0)
  }
  
  res <- hgTransformerGetZeroShot(
    sequences = texts,
    candidate_labels = candidate_labels,
    hypothesis_template = template,
    multi_label = FALSE,
    model = model,
    device = device,
    tokenizer_parallelism = FALSE, # To be checked!
    logging_level = 'error',
    force_return_results = FALSE,
    set_seed = seed
  )
  
  res_wide <- res |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(
      id_cols = sequence,
      names_from = labels,
      values_from = scores
    ) |>
    dplyr::select(sequence, dplyr::all_of(candidate_labels))
  
  if (length(texts) > 1) {
    res_wide <- res_wide |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(candidate_labels), aggregation)
      )
  }
  
  res_wide |>
    dplyr::mutate(
      score = sum(
        dplyr::c_across(dplyr::all_of(candidate_labels)) * mask
      ) / sum(mask > 0)
    ) |>
    dplyr::pull(score)
}

.generate_scale_description <- function(scale, name, max_items = NULL) {
  markers <- purrr::map(scale, names)
  items_neg <- purrr::map_chr(markers, 1)
  items_pos <- purrr::map_chr(markers, 3)
  if (is.null(max_items)) max_items <- length(items_neg)
  if (max_items == 1L) {
    items_neg <- items_neg[1]
    items_pos <- items_pos[1]
  }
  if (length(items_neg) > 1) {
    # Негативные маркеры — дизъюнкция
    items_neg <- items_neg[1:max_items] |> or() |> str_parenthesise()
  }
  if (length(items_pos) > 1) {
    # Позитивные маркеры — конъюнкция
    items_pos <- items_pos[1:max_items] |> and() |> str_parenthesise()
  }
  glue::glue('"{name}": ', paste(items_pos, items_neg, sep = ' vs. '))
}

.generate_schema <- function(scaleset) {
  type_scale <- ellmer::type_enum(
    description = 'Шкала оценки продукта (evaluation scale). Укажите название шкалы, по которой оценивается продукт.',
    values = names(scaleset),
    required = TRUE
  )
  type_rating <- ellmer::type_number(
    description = 'Оценка продукта в отзыве по данной семантической шкале. Значение должно находиться в диапазоне от -2 (очень отрицательная оценка) до 2 (очень положительная оценка). При недостатке информации по данной шкале поставьте 0.',
    required = TRUE
  )
  type_scale_comment <- ellmer::type_string(
    description = 'Комментарий, объясняющий вашу оценку на основе отзыва. Опишите, что вы поняли из текста о мнении пользователя относительно продукта.',
    required = FALSE
  )
  
  type_scale_eval <- ellmer::type_object(
    .description = 'Оценка продукта по семантической шкале (evaluation scale) на основе отзыва. Каждый объект должен содержать название шкалы, оценку и, при необходимости, комментарий. Оценка должна основываться исключительно на предоставленном тексте.',
    scale = type_scale,
    rating = type_rating,
    comment = type_scale_comment,
    .required = TRUE
  )
  
  ellmer::type_array(
    description = 'Массив оценок продукта по каждой из необходимых семантических шкал (evaluation scales). Каждая шкала должна быть оценена ровно один раз.',
    items = type_scale_eval,
    required = TRUE
  )
}

generate_prompts <- function(
    scaleset,
    system_prompt_template,
    user_prompt_template,
    product_type,
    max_items = NULL,
    sample_items = FALSE
) {
  ## Файл или текст? ----
  system_prompt_template <- read(system_prompt_template)
  user_prompt_template <- read(user_prompt_template)
  
  ## User ----
  n_scales <- length(scaleset)
  scale_names <- and(tolower(names(scaleset)))
  
  ## System ----
  ### Описание шкал ----
  ### Если плейсхолдера нет, то не надо ничего обрабатывать
  scaleset_description_required <- any(
    stringr::str_detect(
      c(system_prompt_template, user_prompt_template),
      stringr::fixed('{scaleset_description}')
    )
  )
  
  ### Если есть, построим описание набора шкал
  if (scaleset_description_required) {
    scaleset_description <- purrr::map2(
      scaleset,
      names(scaleset),
      .generate_scale_description,
      max_items = max_items
    ) |>
      paste(collapse = ', ')
  }
  
  ### Если есть плейсхолдер, используем `glue`
  if (scaleset_description_required
  ) {
    system_prompt_template <- ellmer::interpolate(system_prompt_template)
  }
  
  ## Schema ----
  semdiff_schema <- .generate_schema(scaleset = scaleset)
  
  ## Сборка ----
  prompts <- list(
    system = system_prompt_template,
    user = ellmer::interpolate(user_prompt_template),
    schema = semdiff_schema
  )
  
  ## Проверка ----
  stopifnot(inherits(prompts, 'list'))
  return(prompts)
}

.tbl_response <- function(response) {
  response |>
    purrr::map(purrr::compact) |>
    purrr::map(tibble::as_tibble) |>
    dplyr::bind_rows()
}

.empty_tibble <- function(
    scale_names,
    comment = 'Error: Timeout was reached'
) {
  tibble::tibble(
    scale = scale_names,
    rating = -.2,
    comment = comment
  )
}

extract_data <- function(
    task,
    schema,
    model,
    system_prompt,
    seed = 111L,
    temperature = 0
) {
  
  seed <- as.numeric(seed)
  chat <- chat_ollama(
    system_prompt = system_prompt,
    model = model,
    api_args = list(temperature = temperature, seed = seed)
  )
  
  tryCatch(
    {chat$chat_structured(
      task,
      type = schema,
      convert = FALSE,
      # A fix to get round the time-out error
      echo = 'text'
    ) |> .tbl_response()},
    error = function(e) .empty_tibble(schema@items@properties$scale@values)
  )
}

extract_data_map <- function(texts, model, prompts, ...) {
  tasks <- purrr::map(
    texts,
    ~ ellmer::interpolate(prompts$user, text = .)
  )
  print(tasks)
  
  purrr::map(
    tasks,
    function(task) {
      extract_data(
        task,
        schema = prompts$schema,
        model = model,
        system_prompt = prompts$system,
        ...
      )
    }
  )
}

.list_response <- function(response, scale_order, scale_magnitude = 5) {
  if (nrow(response[[1]]) == 0) {
    response <- .empty_tibble(
      scale_order,
      comment = 'Error: Zero length response'
    ) 
  }
  
  result <- dplyr::bind_rows(response, .id = 'text_id') |>
    dplyr::rename(.score = rating) |>
    dplyr::mutate(
      scale = factor(scale, levels = scale_order),
      .score = .score / scale_magnitude
    ) |>
    dplyr::group_by(scale) |>
    dplyr::group_split(.keep = TRUE) |>
    as.list()
  
  result_scale_names <- purrr::map_chr(result, ~as.character(.x[['scale']][1]))
  
  if (!all(result_scale_names == scale_order)) {
    warning('The model has returned improper scale names')
  }
  
  result |> purrr::set_names(result_scale_names)
}
