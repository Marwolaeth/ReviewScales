library(tokenizers)
library(stringr)
library(readr)

compiler::enableJIT(3)

GLUE_PATTERN_NLI <- stringr::regex(
  '\\{\\s*([^}]+?)\\s*\\}',
  comments = FALSE
)

GLUE_PATTERN_CHAT <- stringr::regex(
  '\\{\\{\\s*([^}]+?)\\s*\\}\\}',
  comments = FALSE
)

## Utils ----

like_path <- function(string) {
  (
    (
      stringr::str_detect(string, stringr::fixed('\\')) |
        stringr::str_detect(string, stringr::fixed('/'))
    ) |
      stringr::str_length(string) <= 90
  ) &
    stringr::str_length(string) <= 260 &
    stringr::str_detect(string, '\\.[A-Za-z]{1,3}$')
}

read <- function(string) {
  if (like_path(string)) {
    result <- tryCatch(
      readr::read_file(string),
      error = function(e) string
    )
  } else {
    result <- string
  }
  
  return(result)
}
read <- compiler::cmpfun(read, options = list(optimize=3))

# Функция для быстрого заключения строки в скобки/кавычки/и т.д.
str_enclose <- function(s, enclosure = c('(', ')')){
  if (enclosure[1] == '(')   enclosure <- c(enclosure, ')')
  if (enclosure[1] == '((')  enclosure <- c(enclosure, '))')
  if (enclosure[1] == '[')   enclosure <- c(enclosure, ']')
  if (enclosure[1] == '[[')  enclosure <- c(enclosure, ']]')
  if (enclosure[1] == '[[[') enclosure <- c(enclosure, ']]]')
  if (enclosure[1] == '{')   enclosure <- c(enclosure, '}')
  if (enclosure[1] == '{{')  enclosure <- c(enclosure, '}}')
  if (enclosure[1] == '<')   enclosure <- c(enclosure, '>')
  if (enclosure[1] == '<<')  enclosure <- c(enclosure, '>>')
  if (enclosure[1] == '>')   enclosure <- c(enclosure, '<')
  if (enclosure[1] == '«')   enclosure <- c(enclosure, '»')
  if (enclosure[1] == '‘')   enclosure <- c(enclosure, '’')
  if (enclosure[1] == '“')   enclosure <- c(enclosure, '”')
  paste0(enclosure[1], s, enclosure[length(enclosure)])
}
str_enclose <- compiler::cmpfun(str_enclose, options = list(optimize=3))

str_parenthesise <- function(s){
  paste0('(', s, ')')
}
str_parenthesise <- compiler::cmpfun(
  str_parenthesise,
  options = list(optimize=3)
)

str_keep_allowed <- function(match, allowed, pattern) {
  var <- stringr::str_replace_all(match, pattern, '\\1') |>
    stringr::str_trim(side = 'both')
  ifelse(var %in% allowed, match, '')
}

str_remove_unauthorized <- function(prompt, allowed, double = TRUE) {
  if (double) {
    pttrn = GLUE_PATTERN_CHAT
  } else {
    pttrn = GLUE_PATTERN_NLI
  }
  
  stringr::str_replace_all(
    prompt,
    pattern = pttrn,
    replacement = function(s) str_keep_allowed(s, allowed = allowed, pttrn)
  )
}

and <- function(x) {
  if (length(x) <= 1) return(x)
  delims <- c(rep(',', length(x) - 2), ' и', '')
  paste(paste0(x, delims), collapse = ' ')
}
and <- compiler::cmpfun(and, options = list(optimize = 3))

or <- function(x) {
  if (length(x) <= 1) return(x)
  delims <- c(rep(',', length(x) - 2), ' или', '')
  paste(paste0(x, delims), collapse = ' ')
}
or <- compiler::cmpfun(or, options = list(optimize = 3))

# Функция для токенизации текста на параграфы
#' Токенизация текста на параграфы
#'
#' Эта функция принимает текст в виде строки и разбивает его на параграфы,
#' используя символы новой строки для определения границ параграфов.
#'
#' @param text Строка, содержащая текст, который необходимо токенизировать.
#'             Ожидается, что параграфы отделены символом новой строки ('\n').
#'
#' @return Возвращает вектор параграфов, полученных из входного текста.
#'         Если текст пустой, возвращается пустой вектор.
#'
#' @examples
#' # Пример использования функции
#' text <- "Первый параграф.\nВторой параграф.\n\nТретий параграф."
#' paragraphs(text)
#' # Возвращает: c("Первый параграф.", "Второй параграф.", "Третий параграф.")
#'
#' # Пример с пустым текстом
#' paragraphs("")
#' # Возвращает: character(0)
#'
#' @export
paragraphs <- function(text) {
  tokenizers::tokenize_paragraphs(
    text,
    paragraph_break = '\n',
    simplify = TRUE
  )
}
paragraphs <- compiler::cmpfun(paragraphs, options = list(optimize=3))

sentences <- function(text) {
  tokenizers::tokenize_sentences(
    text,
    simplify = TRUE
  )
}
sentences <- compiler::cmpfun(sentences, options = list(optimize=3))

.check_scale <- function(scale) {
  proper_format <- vapply(
    scale,
    \(x) !any(is.null(names(x))) & all(is.numeric(x)),
    logical(1)
  ) |> all()
  
  if (!proper_format) {
    stop('The labels must be named numeric vectors')
  }
}

softmax <- function(x) exp(x) / sum(exp(x))
softmax <- compiler::cmpfun(softmax, options = list(optimize=3))

