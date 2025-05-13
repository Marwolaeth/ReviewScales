library(purrr)
library(tibble)
library(tidyr)
library(dplyr)

## Wrapper Functions ----
# Функция для обработки нескольких текстов с нулевым обучением
# Функция для семантического дифференциала с нулевым обучением (векторизированная)
#' Семантический дифференциал с нулевым обучением (векторизированный)
#'
#' Эта функция выполняет анализ текста с использованием метода семантического
#' дифференциала, применяя модель нулевого обучения для классификации текстов
#' по заданным полярностям. В отличие от функции `semdiff_zeroshot()`, где
#' аргумент `candidate_labels` используется для указания возможных меток,
#' в этой функции аргумент `items` представляет собой список именованных
#' числовых векторов, где имена служат потенциальными метками классов, а
#' значения используются в качестве маски для расчета результата.
#'
#' @param texts Вектор строк, содержащий тексты для анализа.
#' @param model Строка, указывающая модель, используемую для анализа.
#' @param items Список именованных числовых векторов, содержащих полярности,
#'                   которые будут оцениваться для каждого текста. Имена векторов
#'                   служат метками классов, а значения — маской для расчета результата.
#' @param template Шаблон гипотезы, который будет использоваться для классификации.
#' @param prefix Логическое значение, указывающее, следует ли добавлять префикс
#'                к текстам перед классификацией. По умолчанию FALSE.
#' @param ... Прочие аргументы, передаваемые в `semdiff_zeroshot()`.
#'
#' @return Возвращает дата-фрейм, содержащий для каждого текста
#'         усредненные оценки по всем указанным полярностям. Каждая строка
#'         соответствует тексту, а переменная `.score` – оценке данного текста.
#'         Результат можно интерпретировать
#'         как обобщенные оценки текстов:
#'         - Положительные значения указывают на более высокую оценку по
#'           положительным полярностям.
#'         - Отрицательные значения указывают на более высокую оценку по
#'           отрицательным полярностям.
#'         - Значения близкие к нулю могут указывать на сбалансированное
#'           восприятие текста по рассматриваемым полярностям.
#'
#' @examples
#' # Пример использования функции
#' texts <- c("Это новый и интересный продукт.", "Старая модель неэффективна.")
#' model <- "model_name"
#' items <- list(
#'   c('устаревший' = -1, 'сдержанный' = 0, 'инновационный'   = 1),
#'   c('отсталый'   = -1, 'стабильный' = 0, 'изобретательный' = 1)
#' )
#' template <- "Этот продукт является {}."
#'
#' result <- semdiff_zeroshot_map(
#'   texts = texts,
#'   model = model,
#'   items = items,
#'   template = template,
#'   prefix = TRUE
#' )
#' print(result)
#'
#' @export
semdiff_zeroshot_map <- function(
    texts,
    model,
    items,
    template,
    prefix = FALSE,
    ...
) {
  
  # Можно добавить несколько наборов меток классов
  ## Тогда их придется обрабатывать последовательно
  if (!is.list(items)) items <- list(items)
  
  .check_scale(items)
  
  # Текстам нужен свой ID
  ids <- tibble::tibble(
    texts = texts,
    text_id = 1:length(texts)
  )
  
  # Генерация всех комбинаций текстов и полярностей
  tidyr::expand_grid(
    texts, items
  ) |>
    dplyr::mutate(
      mask = items,
      items = lapply(items, names),
      .score = purrr::pmap_dbl(
        list(texts, items, mask),
        function(texts, items, mask) {
          semdiff_zeroshot(
            texts = paragraphs(texts),
            model = model,
            candidate_labels = items,
            template = template,
            mask = mask,
            multi_label = FALSE,
            prefix = prefix,
            ...
          )
        }
      )
    )
  
  # Объединение результатов с идентификаторами текстов
  # res <- analysis_grid |>
  #   dplyr::left_join(ids, by = 'texts') |>
  #   dplyr::group_by(text_id) |>
  #   dplyr::summarise(across(.score, mean)) |>
  #   dplyr::left_join(ids, by = 'text_id') |>
  #   dplyr::relocate(texts, .after = 1)
}

semdiff_similarity <- function(
    text_embeddings,
    norm_embeddings,
    similarity_metric = 'cosine',
    use_softmax = FALSE,
    temperature = 10,
    ...
) {
  
  # Number of differential items un the scale
  n_items <- length(norm_embeddings$texts)
  
  # Number of item pairs
  n_comparisons <- n_items / 2
  
  similarity_matrix <- similarity_norm(
    text_embeddings$texts$texts,
    norm_embeddings,
    metric = similarity_metric
  )
  
  max_bipolar_scores <- apply(similarity_matrix, 2, max, na.rm = TRUE)
  
  if (use_softmax) {
    max_bipolar_scores <- softmax(max_bipolar_scores * temperature)
    # Summation pattern
    mask <- rep(c(0, 1), n_comparisons)
  } else {
    mask <- rep(c(-1, 1), n_comparisons)
  }
  
  score <- ((max_bipolar_scores %*% mask) / n_comparisons)[1]
  
  tibble::tibble(
    items = paste(names(norm_embeddings$texts), collapse = ' – '),
    .score = score
  )
}

semdiff_chat <- function(
    texts,
    model,
    prompts,
    scale_names,
    scale_magnitude = 5,
    ...
) {
  response <- extract_data_map(
    texts = texts,
    prompts = prompts,
    model = model,
    ...
  )
  
  response |>
    .list_response(
      scale_order = scale_names,
      scale_magnitude = scale_magnitude
    ) |>
    set_names(scale_names)
}
