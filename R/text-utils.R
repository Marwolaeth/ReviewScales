library(reticulate)
library(text)
library(purrr)
library(tibble)
library(dplyr)

## Text Utils ----
reticulate::source_python(
  system.file(
    'python',
    'huggingface_Interface3.py',
    package = 'text',
    mustWork = TRUE
  )
)

#' Concatenate tokens from transformer embeddings into words
#'
#' @param embeddings A data frame containing token embeddings with at least a 'tokens' column and embedding columns named according to dimension names from the `text` package.
#' @param aggregation A function or character string specifying the aggregation method .
#                ('mean', 'min', 'max', 'sum'). Default is mean.
#'
#' @return A data frame with concatenated tokens and aggregated embeddings.
#' @export
#'
#' @examples 
#' library(text)
#' embeddings <- textEmbed('I adore dogs')
#' tokens <- embeddings$tokens$texts[[1]]
#' concatenate_tokens(tokens, 'mean')
.concatenate_tokens <- function(token_embeddings, aggregation = mean) {
  # Check if the embeddings table is empty
  if (nrow(token_embeddings) == 0) stop('The embeddings table is empty')
  
  # Check if the provided aggregation function is valid
  if (is.character(aggregation)) {
    aggregation <- match.arg(aggregation, c('mean', 'min', 'max', 'sum'))
  }
  aggregation <- match.fun(aggregation)
  
  
  is_bert <- token_embeddings$tokens[[1]] == '[CLS]'
  
  special_token_start <- if_else(is_bert, '[', '<')
  word_start <- c('Ġ', '▁')
  subword_indicator <- '#'
  
  result <- token_embeddings |>
    dplyr::mutate(
      first_symbol = substr(tokens, 1, 1),
      special = first_symbol == special_token_start,
      empty = tokens %in% word_start,
      begin = (
        (first_symbol %in% word_start) |
          (!(first_symbol == subword_indicator) & is_bert)
      ) |
        special |
        dplyr::lag(special) |
        empty |
        dplyr::lag(empty),
      group = cumsum(begin),
      tokens = gsub('#+', '', tokens)
    )
  
  if (all(result$begin)) {
    result <- result |>
      dplyr::select(dplyr::all_of(names(token_embeddings)))
  } else {
    result <- result |>
      dplyr::summarise(
        # Concatenate tokens into words
        tokens = paste(tokens, collapse = ''),   
        # Aggregate embeddings
        dplyr::across(
          dplyr::starts_with('Dim'),
          aggregation
        ),
        .by = group
      ) |>
      dplyr::select(-group)
  }
  
  return(result)
}

text_embed_raw <- function(
    texts,
    model,
    device = 'cpu',
    tokenizer_parallelism = FALSE,
    trust_remote_code = TRUE,
    logging_level = 'error',
    max_token_to_sentence = 50
) {
  layers <- get_number_of_hidden_layers(
    model,
    trust_remote_code = trust_remote_code
  )
  
  data_character_variables <- text:::select_character_v_utf8(texts)
  
  x <- data_character_variables
  sorted_layers_ALL_variables <- list()
  sorted_layers_ALL_variables$context_tokens <- list()
  # Loop over all character variables; i_variables = 1
  for (i_variables in seq_len(length(data_character_variables))) {
    # Python file function to HuggingFace
    hg_embeddings <- hgTransformerGetEmbedding(
      text_strings = x[[i_variables]],
      model = model,
      layers = layers,
      return_tokens = TRUE,
      device = reticulate::r_to_py(device),
      tokenizer_parallelism = tokenizer_parallelism,
      model_max_length = NULL,
      max_token_to_sentence = max_token_to_sentence,
      hg_gated = FALSE,
      hg_token = Sys.getenv('HUGGINGFACE_TOKEN', unset = ''),
      trust_remote_code = trust_remote_code,
      logging_level = logging_level
    )
    
    variable_x <- text:::sortingLayers(
      x = hg_embeddings,
      layers = layers,
      return_tokens = TRUE
    )
    
    sorted_layers_ALL_variables$context_tokens[[i_variables]] <- variable_x
    names(
      sorted_layers_ALL_variables$context_tokens
    )[[i_variables]] <- names(x)[[i_variables]]
  }
  
  return(sorted_layers_ALL_variables)
}

.select_token <- function(embeddings, token = 1L, keep_first = FALSE) {
  if (is.numeric(token)) {
    embeddings <- dplyr::filter(embeddings, token_id == token)
  } else if (is.character(token)) {
    # Add an optional first character for RoBERTa tokenizers
    token_regex <- paste0('^.?', tolower(token), '$')
    embeddings <- dplyr::filter(
      embeddings,
      (token_id == 1 & keep_first) |
        (stringr::str_detect(tokens, token_regex))
    )
  } else {
    stop('`select_token` must be either numeric (integer) or character.')
  }
  
  # if (nrow(embeddings) == 0) {
  #   stop('No relevant tokens found.')
  # }
  
  return(embeddings)
}

text_embed_aggregation <- function(
    word_embeddings_layers,
    aggregation_from_tokens_to_texts = c('cls', 'mean', 'min', 'max', 'token'),
    select_token = NULL,
    keep_token_embeddings = FALSE
) {
  aggregation <- match.arg(
    aggregation_from_tokens_to_texts,
    c('cls', 'mean', 'min', 'max', 'token'),
    several.ok = FALSE
  )
  if (aggregation == 'token' & is.null(select_token)) {
    stop(
      paste(
        'Text level aggregation is set to use one token, but the token',
        'is not provided (`select_token = NULL`).'
      )
    )
  }
  
  # Loop over the list of variables; variable_list_i = 1; variable_list_i = 2; remove(variable_list_i)
  selected_layers_aggregated_tibble <- list()
  tokens_list <- list()
  word_embeddings_layers <- word_embeddings_layers$context_tokens
  for (variable_list_i in seq_len(length(word_embeddings_layers))) {
    
    x <- word_embeddings_layers[[variable_list_i]]
    if (tibble::is_tibble(x)) {
      x <- list(x)
    }
    
    # If provided, keep only the selected token to analyse
    if (!is.null(select_token)) {
      x <- purrr::map(x, .select_token, select_token, aggregation == 'cls')
    }
    
    x <- purrr::map(
      x,
      function(text_i) dplyr::select(text_i, -layer_number, -token_id)
    )
    
    if (is.null(aggregation)) {
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- x
    }
    
    # Aggregate across tokens
    if (!is.null(aggregation)) {
      selected_layers_aggregated <- purrr::map(
        x,
        function(text_i) dplyr::select(text_i, dplyr::starts_with('Dim'))
      )
      
      if (aggregation %in% c('token', 'cls')) {
        # Than the first row is what we actually need
        selected_layers_tokens_aggregated <- purrr::map(
          selected_layers_aggregated,
          function(embedding) dplyr::slice(embedding, 1)
        )
      } else {
        selected_layers_tokens_aggregated <- lapply(
          selected_layers_aggregated,
          text:::textEmbeddingAggregation,
          aggregation = aggregation
        ) 
      }
      # Sort output
      selected_layers_aggregated_tibble[[variable_list_i]] <- dplyr::bind_rows(
        selected_layers_tokens_aggregated
      )
    }
    tokens_list[[variable_list_i]] <- x
  }
  
  names(selected_layers_aggregated_tibble) <- names(word_embeddings_layers)
  names(tokens_list) <- names(word_embeddings_layers)
  
  if (keep_token_embeddings) {
    result <- list(
      tokens = tokens_list,
      texts = selected_layers_aggregated_tibble
    )
  } else {
    result <- list(
      texts = selected_layers_aggregated_tibble
    )
  }
  
  return(result)
}

text_embed <- function(
    texts,
    model,
    aggregation_from_tokens_to_texts = c('cls', 'mean', 'min', 'max', 'token'),
    select_token = NULL,
    keep_token_embeddings = FALSE,
    ...
) {
  embeddings_raw <- text_embed_raw(texts, model, ...)
  
  text_embed_aggregation(
    embeddings_raw,
    aggregation_from_tokens_to_texts = aggregation_from_tokens_to_texts,
    select_token = select_token,
    keep_token_embeddings = keep_token_embeddings
  )
}
