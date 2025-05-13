library(shinycssloaders)
library(shinyhelper)

## UI ----
with_red_spinner <- function(
    ui_element,
    size = 1.8,
    caption = 'Pending Evaluation'
) {
  shinycssloaders::withSpinner(
    ui_element,
    type = 2,
    color = '#d73925',
    color.background = '#ECF0F5',
    hide.ui = FALSE,
    size = size,
    caption = caption
  )
}

with_helper <- function(ui_element, content) {
  shinyhelper::helper(
    ui_element,
    colour = '#d73925',
    type = 'markdown',
    content = content,
    buttonLabel = 'Понятно',
    easyClose = TRUE,
    fade = TRUE
  )
}
