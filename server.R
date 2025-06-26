# SERVER ----
server <- function(input, output, session) {
  observe_helpers(withMathJax = TRUE)
  
  ## Интерфейс ----
  
  ### Проверка ввода ----
  iv <- InputValidator$new()
  
  ### Объект ----
  iv$add_rule('object', sv_required(message = 'Обязательно'))
  
  ### Шаблон гипотезы ----
  iv$add_rule('hypothesis_template', sv_required(message = 'Обязательно'))
  iv$add_rule(
    'hypothesis_template',
    sv_regex(
      pattern = '{product_type}',
      fixed = TRUE,
      message = 'Шаблон должен содержать строки: `{product_type}` и `{hypothesis}`'
    )
  )
  iv$add_rule(
    'hypothesis_template',
    sv_regex(
      pattern = '{hypothesis}',
      fixed = TRUE,
      message =
        'Шаблон должен содержать местозаполнители: `{product_type}` и `{hypothesis}`.'
    )
  )
  
  ### Промпты ----
  iv$add_rule('chat_system_prompt', sv_required(message = 'Обязательно'))
  iv$add_rule('chat_user_prompt', sv_required(message = 'Обязательно'))
  iv$add_rule(
    'chat_user_prompt',
    sv_regex(
      pattern = '{{{{text}}}}',
      fixed = TRUE,
      message = 'Инструкция должна содержать местозаполнитель `{{{{text}}}}`.'
    )
  )
  
  iv$enable()
  
  output$hypothesis_preview <- renderText({
    req(input$object)
    req(input$hypothesis_template)
    
    input$hypothesis_template |>
      str_remove_unauthorized(allowed = ALLOWED_PLACEHOLDERS_NLI) |>
      stringr::str_replace(
        fixed('{product_type}'),
        fixed(input$object)
      ) |>
      stringr::str_replace(
        fixed('{hypothesis}'),
        # Самый первый позитивный маркер
        fixed(names(scaleset()[[1]][[1]])[3])
      )
  })
  
  ### Выбор моделей в зависимости от метода ----
  models <- reactive({
    if (input$method == 'chat') {
      chat_models
    } else {
      models_df |>
        dplyr::filter(
          lang == 'ru' & (task == 'NLI' | input$method == 'similarity') & ok
        ) |>
        dplyr::mutate(id = row_number()) |>
        dplyr::select(model, id) |>
        tibble::deframe()
    }
  })
  
  model_name <- reactive(names(models()[as.numeric(input$model)]))
  
  output$model <- renderUI({
    current_model <- isolate(input$model)
    selectInput(
      'model',
      label = 'Модель',
      choices = models(),
      selected = current_model,
      width = '100%'
    )
  })
  
  ## Случайный пример ----
  observeEvent(input$example, {
    ex <- dplyr::slice_sample(examples, n = 1) |>
     dplyr::pull(text)
    
    updateTextInput(
      session,
      'text',
      value = ex
    )
  })
  
  ## Префикс ----
  prefix <- reactive({
    req(models)
    req(input$model)
    
    # Добавим префиксы, если модель их принимает
    stringr::str_detect(
      model_name(),
      '([Ss]enten)|([Ss][Bb][Ee][Rr][Tt])|(s\\-encoder)'
    )
  })
  
  ## Хранение истории ----
  eval_history <- reactiveVal(tibble::tibble())
  
  ## Обработка ----
  
  ### NLI ----
  #### Шаблон гипотезы ----
  hypotheses <- reactive({
    req(input$object)
    req(input$hypothesis_template)
    
    input$hypothesis_template |>
      str_remove_unauthorized(allowed = ALLOWED_PLACEHOLDERS_NLI) |>
      stringr::str_replace(
        fixed('{product_type}'),
        fixed(tolower(input$object))
      ) |>
      stringr::str_replace(fixed('{hypothesis}'), fixed('{}'))
  })
  
  ### Чат-модели ----
  #### Промпты ----
  ##### Обновление шаблонов ----
  prompt_templates <- reactiveValues(
    system = DEFAULT_SYSTEM_PROMPT_TEMPLATE,
    user = DEFAULT_USER_PROMPT_TEMPLATE
  )
  
  observeEvent(input$generate_prompts, {
    system_prompt <- str_remove_unauthorized(
      input$chat_system_prompt,
      allowed = ALLOWED_PLACEHOLDERS_CHAT
    )
    user_prompt <- str_remove_unauthorized(
      input$chat_user_prompt,
      allowed = ALLOWED_PLACEHOLDERS_CHAT
    )
    
    prompt_templates[['system']] <- system_prompt
    prompt_templates[['user']]   <- user_prompt
  })
  
  ##### Генерация ----
  prompts <- reactive({
    generate_prompts(
      scaleset(),
      system_prompt_template = prompt_templates[['system']],
      user_prompt_template = prompt_templates[['user']],
      product_type = tolower(input$object),
      max_items = 1L
    )
  })
  
  ##### Предпросмотр ----
  output$system_prompt_preview <- renderPrint({
    stringr::str_wrap(prompts()[['system']], width = 80) |> cat()
  })
  
  output$user_prompt_preview <- renderPrint({
    stringr::str_wrap(prompts()[['user']], width = 80) |> cat()
  })
  
  ##### Копирование полной версии ----
  observeEvent(input$chat_copy_preview, {
    system_prompt <- prompts()[['system']]
    user_prompt <- prompts()[['user']] |>
      stringr::str_replace(
        stringr::fixed('{{text}}'),
        stringr::fixed('{{{{text}}}}')
      )
    
    updateTextAreaInput(
      session = session,
      inputId = 'chat_system_prompt',
      value = system_prompt
    )
    updateTextAreaInput(
      session = session,
      inputId = 'chat_user_prompt',
      value = user_prompt
    )
  })
  
  ##### Возврат ----
  observeEvent(input$chat_default_promts, {
    updateTextAreaInput(
      session = session,
      inputId = 'chat_system_prompt',
      value = DEFAULT_SYSTEM_PROMPT_TEMPLATE
    )
    updateTextAreaInput(
      session = session,
      inputId = 'chat_user_prompt',
      value = DEFAULT_USER_PROMPT_TEMPLATE
    )
  })
  
  ##### Экспорт и импорт ----
  output$download_prompts <- downloadHandler(
    filename = function() {
      paste('prompts-', Sys.Date(), '.RData', sep = '')
    },
    content = function(file) {
      prompts <- list(
        system = input$chat_system_prompt,
        user   = input$chat_user_prompt
      )
      save(prompts, file = file)
    }
  )
  observeEvent(input$upload_prompts, {
    req(input$upload_prompts)
    
    prompts_data <- new.env()
    tryCatch({
      load(input$upload_prompts$datapath, envir = prompts_data)
      
      new_promts <- get(ls(prompts_data)[1], envir = prompts_data)
      
      nms <- names(new_promts)
      if (!(('system' %in% nms) & ('user' %in% nms))) {
        showNotification(
          "Файл не содержит необходимые ключи: 'system' и 'user'.",
          type = 'error'
        )
        return()
      }
      
      updateTextAreaInput(
        session = session,
        inputId = 'chat_system_prompt',
        value = new_promts[['system']]
      )
      updateTextAreaInput(
        session = session,
        inputId = 'chat_user_prompt',
        value = new_promts[['user']]
      )
    }, error = function(e) {
      showNotification(
        'Ошибка при загрузке файла: ' + e$message,
        type = 'error'
      )
    })
  })
  
  ## Анализ ----
  result <- reactive({
    req(input$object)
    req(input$model)
    req(input$text)
    req(hypotheses)
    req(prefix)
    
    ### Получение модели ----
    print(model_name())
    
    text <- input$text
    object <- tolower(input$object)
    
    print(text)
    print(hypotheses())
    
    ### Функции анализа ----
    tictoc::tic('Analysing sigle text')
    
    #### Chat models ----
    if (input$method == 'chat') {
      res <- semdiff_chat(
        text,
        model = model_name(),
        prompts = prompts(),
        scale_names = names(scaleset()),
        seed = input$seed,
        temperature = input$chat_temperature,
        scale_magnitude = GAUGE_SCALE
      )
    } else {
      withProgress(
        session = session,
        message = 'Анализируем…',
        {
          res <- purrr::map2(
            scaleset(),
            seq_along(scaleset()),
            function(semantic_scale, i) {
              incProgress(
                1 / length(scaleset()),
                message = 'Анализируем',
                detail = names(scaleset())[[i]]
              )
              #### NLI ----
              if (input$method == 'classification') {
                scale_result <- semdiff_zeroshot_map(
                  text,
                  model_name(),
                  items = semantic_scale,
                  template = hypotheses(),
                  prefix = prefix(),
                  append_neutral = TRUE,
                  seed = input$seed,
                  device = tolower(input$device)
                ) 
              }
              return(scale_result)
            }
          ) |>
            purrr::set_names(names(scaleset()))
        })
    }
    tictoc::toc()
    
    ### Сохранение истории ----
    if (isolate(input$method) == 'classification') {
      current_result <- res |>
        show_scales_result()
    } else {
      current_result <- res
    }
    
    current_result <- current_result |>
      dplyr::bind_rows(.id = 'scale') |>
      dplyr::select(-dplyr::any_of('text_id')) |>
      mutate(
        text = isolate(input$text),
        model = isolate(model_name()),
        .before = 0L
      )
    
    eval_history(
      dplyr::bind_rows(
        isolate(eval_history()),
        current_result
      )
    )
    
    ### Итог ----
    res
  }) |>
    bindCache(
      input$model,
      input$text,
      scaleset(),
      input$method,
      input$similarity_group_items,
      hypotheses(),
      prompts()
    ) |>
    bindEvent(input$submit)
  
  
  ## Вывод ----
  output$result <- renderPrint({
    req(input$submit)
    if (input$method == 'classification') {
      result() |>
        show_scales_result()
    } else {
      result()
    }
  })
  
  output$gauges <- renderUI({
    req(input$submit)
    gauges <- lapply(seq_along(result()), function(scale_i) {
      with_red_spinner(
        gaugeOutput(outputId = paste0('gauge_', scale_i), width = '100%'),
        size = 1.5,
        caption = NULL
      )
    })
    # Generate columns of equal width
    wd = 12 / length(gauges)
    do.call(tagList, lapply(gauges, column, width = wd))
  })
  
  # Генерация тахометров
  observe({
    req(input$submit)
    lapply(seq_along(result()), function(scale_i) {
      scale_name <- names(result())[scale_i]
      output[[paste0('gauge_', scale_i)]] <- renderGauge({
        value <- mean(result()[[scale_name]][['.score']]) * GAUGE_SCALE
        gauge(
          label = scale_name,
          value,
          min = -GAUGE_SCALE, max = GAUGE_SCALE, 
          symbol = '', 
          gaugeSectors(
            success = c(GAUGE_SCALE * .8, GAUGE_SCALE),
            danger = c(-GAUGE_SCALE, -GAUGE_SCALE * .4),
            warning = c(-GAUGE_SCALE * .4, GAUGE_SCALE * .8)
          )
        )
      })
    })
  })
  
  ## Редактирование шкал ----
  ### Исходная шкала ----
  scaleset <- reactiveVal(
    list(
      'Мощность всасывания' = list(
        c('не мощный' = -1, 'средней мощности' = 0, 'мощный'   = 1)
      ),
      'Удобство' = list(
        c('неудобный'  = -1, 'нормальный'       = 0, 'удобный'    = 1),
        c('громоздкий' = -1, 'не очень удобный' = 0, 'компактный' = 1)
      ),
      'Тишина' = list(
        c('шумный' = -1, 'не очень шумный' = 0, 'тихий' = 1)
      ),
      'Качество уборки' = list(
        c(
          'плохо убирает пыль' = -1,
          'убирает средне' = 0,
          'отлично убирает пыль' = 1
        ),
        c(
          'плохо всасывает пыль' = -1,
          'посредственно всасывает пыль' = 0,
          'отлично всасывает пыль' = 1
        ),
        c(
          'плохо пылесосит' = -1,
          'посредственно пылесосит' = 0,
          'дает хорошее качество уборки' = 1
        )
      ),
      'Общая оценка' = list(
        c('плохой' = -1, 'посредственный' = 0, 'отличный' = 1)
      )
    )
  )
  
  ### Экспорт и импорт ----
  output$download_scaleset <- downloadHandler(
    filename = function() {
      paste('scaleset-', Sys.Date(), '.sds', sep = '')
    },
    content = function(file) {
      scaleset <- scaleset()
      save(scaleset, file = file)
    }
  )
  observeEvent(input$upload_scaleset, {
    req(input$upload_scaleset)
    
    # Load the RData file and update the scaleset
    scales_data <- new.env()
    load(input$upload_scaleset$datapath, envir = scales_data)
    
    # Assuming the scaleset is stored as a list in the RData file
    scaleset(get(ls(scales_data)[1], envir = scales_data))
  })
  
  
  ### Редактирование ----
  new_scaleset <- reactiveVal()
  
  output$scale_inputs <- renderUI({
    scales <- scaleset()
    scale_inputs <- lapply(seq_along(scales), function(i) {
      scaleEditorUI(paste0('scale_', i))  # Вызов модуля для каждой шкалы
    })
    do.call(tagList, scale_inputs)
  })
  
  observe({
    lapply(seq_along(scaleset()), function(i) {
      scaleEditorServer(
        paste0('scale_', i),
        i,
        scaleset,
        new_scaleset
      )  # Передача реактивного значения
    })
  })
  
  observeEvent(input$add_scale, {
    scales <- scaleset()
    scales[[paste0('Шкала ', length(scales) + 1)]] <- list(
      c('отрицательная' = -1, 'нейтральная' = 0, 'положительная' = 1)
    )
    scaleset(scales)
  })
  
  observeEvent(input$submit_scales, {
    req(new_scaleset)
    
    scaleset(new_scaleset())
  })
  
  output$scales_output <- renderPrint({
    scaleset()
  })
  
  ## История оценок ----
  output$history <- renderReactable({
    
    eval_history() |>
      reactable(
        defaultColDef = colDef(
          headerStyle = list(background = '#DD4B39')
        ),
        columns = column_definitions,
        elementId = 'history-table'
      )
  })
}
