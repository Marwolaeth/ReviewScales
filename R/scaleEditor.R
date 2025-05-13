# R/scaleEditor.R

# Интерфейс модуля для редактирования шкалы
scaleEditorUI <- function(id) {
  
  ns <- NS(id)  # Создание пространства имен
  
  tagList(
    div(
      style = 'border: 2px solid #ccc; padding: 10px; margin-bottom: 10px;',
      fluidRow(
        column(
          width = 11,
          uiOutput(ns('scale_name_ph'))
        ),
        column(
          1,
          style='padding-left:0px;',
          actionButton(
            ns('delete_scale'),
            label = '',
            title = 'Удалить шкалу',
            icon = icon('trash-can'),
            style = paste(
              'margin-top:25px;margin-right:45px;margin-left:0px',
              'border:none;background:inherit;',
              sep = ';'
            )
          )
        )
      ),
      h5('Характеристики:'),
      uiOutput(ns('items')),
      ## Items ----
      fluidRow(
        column(
          6,
          actionButton(
            ns('add_item'),
            'Добавить характеристики',
            icon = icon('plus')
          )
        ),
        column(
          6,
          style='text-align:right;',
          actionButton(ns('save_scale'), 'Сохранить', class = 'btn-success')
        )
      )
    )
  )
}

# Логика модуля для редактирования шкалы
scaleEditorServer <- function(id, i, scales_reactive, container) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Получение текущих значений ----
    scale <- reactiveVal(scales_reactive()[[i]])  # Получаем текущую шкалу
    scale_name <- names(scales_reactive())[i]  # Имя текущей шкалы
    
    # Интерфейс ----
    ## Название шкалы ----
    output$scale_name_ph <- renderUI({
      textInput(ns('scale_name'), 'Название шкалы', value = scale_name)
    })
    
    ## Items ----
    output$items <- renderUI({
      lapply(seq_along(scale()), function(j) {
        items <- scale()[[j]]
        div(
          style = 'border: 1px solid #ddd; padding: 5px; margin-bottom: 10px;',
          ### Редактирование ----
          fluidRow(
            column(
              4,
              textInput(
                ns(paste0('item_name_', j, '_neg')),
                'Отрицательная',
                value = names(items)[1]
              )
            ),
            column(
              3,
              textInput(
                ns(paste0('item_name_', j, '_neu')),
                'Нейтральная',
                value = names(items)[2]
              )
            ),
            column(
              4,
              textInput(
                ns(paste0('item_name_', j, '_pos')),
                'Положительная',
                value = names(items)[3]
              )
            ),
            ### Удаление ----
            column(
              1,
              style='padding-left:0px;',
              actionButton(
                ns(paste0('delete_item_', j)),
                label = '',
                title = 'Удалить пункт',
                icon = icon('trash-can'),
                onclick = paste0(
                  "Shiny.setInputValue('", ns("delete_item"), "', ", j, ");"
                ),
                style = paste(
                  'margin-top:25px;margin-right:45px;margin-left:0px',
                  'border:none;background:inherit;',
                  sep = ';'
                )
              )
            )
          )
        )
      })
    })
    
    # Редактирование ----
    ## Удаление шкалы ----
    observeEvent(input$delete_scale, {
      new_scaleset <- scales_reactive()
      
      new_scaleset[[i]] <- NULL
      
      container(new_scaleset)
    })
    
    ## Добавление характеристик ----
    observeEvent(input$add_item, {
      new_scale <- scale()
      scale(append(
        new_scale,
        list(
          setNames(
            c(-1, 0, 1), c('негативный', 'нейтральный', 'позитивный')
          )
        )
      ))
      shinyjs::runjs(
        paste0("Shiny.setInputValue('", ns("add_item"), "', ", "null);")
      )
    })
    
    ## Удаление характеристик ----
    observeEvent(input$delete_item, {
      req(input$delete_item)
      # Get the index of the marker to delete from the input
      j <- as.numeric(input$delete_item)
      print(j)
      shinyjs::runjs(
        paste0("Shiny.setInputValue('", ns("delete_item"), "', ", "'');")
      )
      # shinyjs::reset('delete_item')
      
      # Get the current scale
      new_scale <- scale()
      
      # Remove the marker at the specified index
      new_scale[[j]] <- NULL
      
      # Update the scale reactive value
      scale(new_scale)
    })
    
    # Сохранение изменений в шкале ----
    observeEvent(input$save_scale, {
      new_scaleset <- scales_reactive()  # Получаем текущее значение шкал
      new_scale_name <- input$scale_name
      new_items <- list()
      
      for (j in seq_along(scale())) {
        
        neg <- input[[paste0('item_name_', j, '_neg')]]
        neu <- input[[paste0('item_name_', j, '_neu')]]
        pos <- input[[paste0('item_name_', j, '_pos')]]
        
        new_items[[j]] <- purrr::set_names(c(-1, 0, 1), c(neg, neu, pos))
      }
      
      # Обновляем название шкалы и характеристики
      names(new_scaleset)[i] <- new_scale_name  # Обновляем имя шкалы
      new_scaleset[[i]] <- new_items
      
      container(new_scaleset)  # Обновляем реактивное значение
    })
  })
}
