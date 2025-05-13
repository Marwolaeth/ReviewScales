library(tidyr)
library(text)
library(ellmer)

EXAMPLE_GENERAL_PATH <- file.path(
  'data',
  'prompts',
  'semdiff-few-shot-example-general.md'
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

.extract_concepts <- function(items, ids, group = FALSE, sep = ', ') {
  concepts <- lapply(items, names) |>
    purrr::map(\(item) item[ids])
  if (group) {
    concepts <- concepts |>
      purrr::pmap_chr(paste, sep = sep)
  } else {
    concepts <- unlist(concepts)
  }
  
  return(concepts)
}
.extract_concepts <- compiler::cmpfun(.extract_concepts, options = list(optimize=3))

.items_to_norms <- function(
    items,
    model,
    as_phrases = FALSE,
    template = NULL,
    prefix = FALSE,
    group_items = FALSE,
    aggregation = if (as_phrases) 'cls' else 'mean',
    sep = ', ',
    ...
) {
  if (!is.list(items)) items <- list(items)
  
  .check_scale(items)
  
  concepts <- .extract_concepts(items, c(1, 3), group_items, sep)
  
  concept_names <- concepts
  
  if (as_phrases) {
    if (is.null(template)) template <- 'Y – {}'
    concepts <- sapply(
      concepts,
      function(concept) {
        stringr::str_replace(
          template,
          stringr::fixed('{}'),
          stringr::fixed(concept)
        )
      }
    )
  }
  if (prefix) concepts <- paste('classification:', concepts)
  
  concepts_df <- tibble::as_tibble(
    as.list(concepts) |> setNames(concept_names)
  )
  
  text_embed(
    concepts_df,
    model = model,
    keep_token_embeddings = FALSE,
    aggregation_from_tokens_to_texts = aggregation,
    ...
  )
}

similarity_norm <- function(
    text_embeddings,
    norm_embeddings,
    metric = 'cosine'
) {
  metric <- match.arg(metric, c('cosine', 'spearman', 'pearson', 'kendall'))
  
  if (nrow(text_embeddings) == 0) {
    return(matrix(0, nrow = 1, ncol = length(norm_embeddings$texts)))
  }
  
  texts <- text_embeddings |>
    dplyr::select(dplyr::starts_with('Dim')) |>
    as.matrix()
  concepts <- norm_embeddings$texts |>
    purrr::map(as.matrix) |>
    purrr::reduce(rbind)
  
  if (metric == 'cosine') {
    texts_norms <- sqrt(rowSums(texts^2))
    concept_norms <- sqrt(rowSums(concepts^2))
    
    S <- (texts %*% t(concepts)) / (texts_norms %*% t(concept_norms))
  } else {
    S <- cor(t(texts), t(concepts), method = metric)
  }
  
  colnames(S) <- names(norm_embeddings$texts)
  return(S)
}
similarity_norm <- compiler::cmpfun(similarity_norm, options = list(optimize=3))

benchmark_similarity <- function(
    norm_embeddings,
    model,
    template,
    prefix = FALSE,
    aggregation = if (prefix) 'cls' else 'token',
    metric = 'cosine',
    ...
) {
  concepts <- names(norm_embeddings$texts)
  
  hypotheses <- sapply(
    concepts,
    function(concept) {
      stringr::str_replace(
        template,
        stringr::fixed('{}'),
        stringr::fixed(concept)
      )
    }
  )
  
  if (prefix) hypotheses <- paste('classification:', hypotheses)
  
  hypotheses_embeddings <- text_embed(
    hypotheses,
    model,
    aggregation_from_tokens_to_texts = aggregation,
    ...
  )
  
  score <- diag(
    similarity_norm(
      hypotheses_embeddings$texts$texts,
      norm_embeddings,
      metric = metric
    )
  )
  names(score) <- concepts
  return(score)
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

.generate_scale_example <- function(
    scale,
    name,
    rating = NULL,
    sample_items = FALSE
) {
  markers <- purrr::map(scale, names)
  # If rating is not given, it is random
  if (!is.null(rating)) {
    if (is.na(rating)) {
      rating <-  NULL
    }
  }
  if (is.null(rating)) {
    rating <- sample(
      c(-5L, -4L, -3L, 0L, 3L, 4L, 5L),
      size = 1,
      prob = c(1, 1, 1, 2, 1, 1, 1)
    )
  }
  items_neg <- purrr::map_chr(markers, 1)
  items_pos <- purrr::map_chr(markers, 3)
  if (sample_items) {
    item_neg <- sample(items_neg, size = 1)
    item_pos <- sample(items_pos, size = 1)
  } else {
    item_neg <- items_neg[[1]]
    item_pos <- items_pos[[1]]
  }
  
  if (rating  == 0) {
    comment <- glue::glue(
      'The text provides no relevant info concerning the scale «{name}»'
    )
    example_text <- glue::glue(
      'Мне нечего сказать про {universal_brand_name}.'
    )
  } else {
    if (rating < 0) {
      marker <- item_neg
    } else {
      marker <- item_pos
    }
    
    # Если характеристика содержит слово «бренд», не повторяем его
    if (stringr::str_detect(marker, stringr::fixed('бренд'))) {
      denoter <- 'это'
    } else {
      denoter <- 'этот бренд'
    }
    
    # Если в характеристике явное отрицание
    if (stringr::str_detect(marker, stringr::fixed('не ')) & rating < 0) {
      intensifier <- 'совсем'
    } else {
      intensifier <- 'очень'
    }
    
    quantifier <- switch(
      abs(rating) - 2,
      'скорее',
      'достаточно',
      intensifier
    )
    
    comment <- glue::glue(
      'Автор текста считает, что {denoter} {quantifier} {marker}'
    )
    # One Edge Case
    comment <- stringr::str_replace(
      comment,
      stringr::fixed('достаточно не '),
      stringr::fixed('не достаточно ')
    )
    
    # Эпистемическая модальность
    modal <- sample(
      c(
        'Я уверен',
        'Я думаю',
        'Я считаю',
        'Мне кажется',
        'Я полагаю',
        'Все знают',
        'Всем известно',
        'Мое мнение',
        'Я убежден'
      ),
      size = 1
    )
    
    example_text <- glue::glue(
      '{modal}, что {universal_brand_name} — {quantifier} {marker}'
    )
  }
  response <- glue::glue(
    '{{"scale":"{name}","rating":{rating},"comment":"{comment}"}}'
  )
  
  # glue::glue(
  #   '- text: {example_text}\n  response: {response}',
  #   .trim = FALSE
  # )
  list(text = example_text, response = response)
}

.generate_scaleset_example <- function(scaleset, sample_items = FALSE) {
  ratings <- rep(0, length(scaleset))
  which_scale <- sample(1:length(scaleset), size = 1)
  # One scale is not missing
  ratings[which_scale] <- sample(3:5, size = 1) * sample(c(-1, 1), size = 1)
  
  examples <- purrr::pmap(
    list(
      scale = scaleset,
      name = names(scaleset),
      rating = ratings
    ),
    .generate_scale_example,
    sample_items = sample_items
  )
  
  example_response <- purrr::map(examples, 'response') |>
    paste(collapse = ',') |>
    str_enclose('[') |> jsonlite::prettify(indent = 2)
  
  example_text <- purrr::map(examples, 'text') |>
    purrr::pluck(which_scale)
  
  glue::glue(
    '- text: {example_text}\n  response: {example_response}',
    .trim = FALSE
  )
}

.generate_schema <- function(scaleset) {
  type_scale <- ellmer::type_enum(
    description = 'Шкала оценки имиджа бренда (evaluation scale). Укажите название шкалы, по которой вы оцениваете имидж бренда "Y", отраженный в тексте.',
    values = names(scaleset),
    required = TRUE
  )
  type_rating <- ellmer::type_number(
    description = 'Оценка имиджа бренда "Y" по данной семантической шкале. Значение должно находиться в диапазоне от -5 (очень отрицательная оценка) до 5 (очень положительная оценка). При недостатке информации о бренде "Y" в данном тексте поставьте 0.',
    required = TRUE
  )
  type_scale_comment <- ellmer::type_string(
    description = 'Комментарий, объясняющий вашу оценку для бренда "Y". Опишите, что вы поняли из текста о мнении автора относительно бренда и как бренд представлен в тексте.',
    required = FALSE
  )
  
  type_scale_eval <- ellmer::type_object(
    .description = 'Оценка для бренда "Y" по семантической шкале (evaluation scale). Каждый объект должен содержать название шкалы, оценку и, при необходимости, комментарий. Оценка должна основываться исключительно на предоставленном тексте.',
    scale = type_scale,
    rating = type_rating,
    comment = type_scale_comment,
    .required = TRUE
  )
  
  ellmer::type_array(
    description = 'Массив оценок имиджа бренда "Y" по каждой из необходимых семантических шкал (evaluation scales). Каждая шкала должна быть оценена ровно один раз.',
    items = type_scale_eval,
    required = TRUE
  )
}

generate_prompts <- function(
    scaleset,
    system_prompt_template,
    user_prompt_template,
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
  
  ### Пример оценки шкал ----
  ### Если автоматический пример ответа не требуется, то не надо его составлять
  scaleset_example_required <- any(
    stringr::str_detect(
      c(system_prompt_template, user_prompt_template),
      stringr::fixed('{scaleset_example}')
    )
  )
  
  ### Если требуется, то надо составить
  if (scaleset_example_required) {
    scaleset_example <- .generate_scaleset_example(scaleset, sample_items)
    ### Если нет автоматического примера ответа, то, возможно, формат не прописан
  }
  
  ### Пример оценки общий ----
  format_example_required <- any(
    stringr::str_detect(
      c(system_prompt_template, user_prompt_template),
      stringr::fixed('{general_example}')
    )
  )
  general_example <- ellmer::interpolate_file(EXAMPLE_GENERAL_PATH)
  
  if (!format_example_required) {
    #### Проверим, прописан ли формат
    format_example_provided <- stringr::str_detect(
      system_prompt_template,
      '\\"rating\\"\\:'
    ) & stringr::str_detect(
      system_prompt_template,
      '\\"comment\\"\\:'
    )
    #### Если не прописан, добавим
    if (!format_example_provided) {
      ##### Добавим описание формата в конец инструкции
      system_prompt_template <- paste(
        system_prompt_template,
        '**Examples:**',
        general_example,
        sep = '\n'
      )
    }
  }
  
  ### Если есть хотя бы один плейсхолдер, используем `glue`
  if (
    scaleset_example_required |
    scaleset_description_required |
    format_example_required
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
    seed = seed,
    api_args = list(temperature = temperature, timeout = 1200)
  )
  
  tryCatch(
    {chat$extract_data(
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
