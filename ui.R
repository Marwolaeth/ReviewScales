library(shiny)
library(flexdashboard)
library(shinydashboard)
library(shinydashboardPlus)

# UI ----
ui <- dashboardPage(
  title = 'Brand Semantics',
  skin = 'red',
  
  header = dashboardHeader(title = 'Brand Semantics', titleWidth = '25%'),
  
  ## Боковая панель ----
  sidebar = dashboardSidebar(
    width = '30%',
    sidebarMenu(
      menuItem('Оценка', tabName = 'assessment', icon = icon('check')),
      menuItem('Шкалы', tabName = 'settings', icon = icon('sliders')),
      menuItem(
        'Настройки оценщиков',
        tabName = 'method-settings',
        icon = icon('magnifying-glass-chart')
      ),
      menuItem('История', tabName = 'history', icon = icon('clock-rotate-left'))
    )
  ),
  
  ## Панель настроек ----
  controlbar = dashboardControlbar(
    width = 450,
    tags$div(
      style = 'margin:10px;',
      radioButtons(
        'method',
        label = 'Метод оценки',
        choices = list(
          'Нулевая классификация' = 'classification',
          'Искусственный интеллект' = 'chat'
        ),
        selected = 'classification'
      ) |> with_helper('methods'),
      uiOutput('model'),
      radioButtons(
        'device',
        label = 'Устройство',
        choices = c('CPU', 'GPU'),
        selected = 'CPU'
      ),
      numericInput(
        'seed',
        label = 'Начальное случайное значение',
        min = 1,
        max = 11111,
        value = 111
      ) |> with_helper('seed')
    )
  ),
  
  ## Основная панель ----
  body = dashboardBody(
    useShinyjs(),
    
    tabItems(
      ### Интерфейс оценки ----
      tabItem(
        tabName = 'assessment',
        #### Ввод ----
        fluidRow(
          column(
            12,
            fluidRow(
              column(
                8,
                textInput(
                  'object',
                  'Тип продукта',
                  value = 'пылесос',
                  width = '100%',
                  placeholder = paste(
                    'Объекты через запятую, например:',
                    '«Бренд А, наша компания, мы»'
                  )
                )
              ),
              column(
                4,
                actionButton(
                  'example', 'Случайный пример', class = 'btn-warning',
                  style = 'margin-top:25px;'
                )
              )
            ),
            textAreaInput(
              'text',
              'Текст:',
              '',
              rows = 5,
              resize = 'vertical',
              width = '100%'
            ),
            actionButton(
              'submit',
              'Оценить',
              class = 'btn-success',
              icon = icon('check-double')
            )
          )
        ),
        #### Вывод ----
        fluidRow(
          style = 'padding:10px;',
          tabsetPanel(
            id = 'scales_output',
            tabPanel(
              'Результат',
              value = 'scales_output_visual',
              fluidRow(uiOutput('gauges')),
            ),
            tabPanel(
              'Просмотр расчетов',
              value = 'scales_output_raw',
              with_red_spinner(
                verbatimTextOutput('result', placeholder = TRUE),
                size = 2,
                caption = NULL
              )
            )
          )
        )
      ),
      ### Настройка шкал ----
      tabItem(
        tabName = 'settings',
        h2('Редактирование семантических шкал'),
        #### Редактирование ----
        tabsetPanel(
          tabPanel(
            'Редактирование',
            value = 'edit-scales',
            uiOutput('scale_inputs'),
            #### Добавить и Сохранить ----
            fluidRow(
              column(
                6,
                actionButton(
                  'add_scale',
                  'Добавить шкалу',
                  class = 'btn-primary btn-lg',
                  icon = icon('plus')
                )
              ),
              column(
                6,
                style='text-align:right;',
                actionButton(
                  'submit_scales',
                  'Сохранить',
                  class = 'btn-success btn-lg',
                  icon = icon('save')
                )
              )
            )
          ),
          #### Предпросмотр ----
          tabPanel(
            'Предпросмотр',
            value = 'edit-scales-preview',
            verbatimTextOutput('scales_output')
          )
        ),
        tags$br(),
        #### Экспорт и импорт ----
        fluidRow(
          column(
            6,
            downloadButton(
              'download_scaleset',
              'Экспортировать шкалы',
              class = 'btn-success',
              style = 'margin-top:25px;'
            )
          ),
          column(
            6,
            fileInput(
              'upload_scaleset',
              'Импортировать шкалы',
              accept = c('.sds', '.RData'),
              buttonLabel = 'Обзор…',
              placeholder = 'Файл не выбран'
            )
          )
        )
      ),
      ### Настройка оценщиков ----
      tabItem(
        tabName = 'method-settings',
        h2('Настройка оценщиков'),
        #### NLI ----
        tabsetPanel(
          tabPanel(
            'Классификация',
            value = 'edit-classification',
            tags$br(),
            ##### Шаблон гипотезы ----
            textInput(
              'hypothesis_template',
              'Шаблон гипотезы',
              value = 'Этот {brand_name} {hypothesis}'
            ) |> with_helper('hypothesis-template'),
            h5('Предпросмотр: '),
            tags$div(
              textOutput(
                'hypothesis_preview',
                inline = FALSE
              ),
              style = 'color:#888;'
            )
          ),
          #### Chat ----
          tabPanel(
            'Чат-модели',
            value = 'edit-chat',
            
            ##### Температура ----
            fluidRow(
              column(
                6,
                h4('Температура') |> with_helper('temperature'),
                sliderInput(
                  'chat_temperature',
                  '',
                  min = 0,
                  max = 1,
                  value = 0,
                  step = .1
                )
              ),
              column(6)
            ),
            
            ##### Редактирование промптов ----
            fluidRow(
              column(
                6,
                h4('Редактирование инструкций') |> with_helper('prompts'),
                textAreaInput(
                  inputId = 'chat_system_prompt',
                  label = 'Системная инструкция',
                  value = DEFAULT_SYSTEM_PROMPT_TEMPLATE,
                  rows = 12,
                  width = '100%'
                ),
                textAreaInput(
                  inputId = 'chat_user_prompt',
                  label = 'Пользовательская инструкция',
                  value = DEFAULT_USER_PROMPT_TEMPLATE,
                  rows = 6,
                  width = '100%'
                ),
                ##### Кнопки ----
                actionButton(
                  'generate_prompts',
                  'Применить изменения',
                  icon = icon('bolt'),
                  width = '100%',
                  style = 'margin-top:6px;'
                ),
                actionButton(
                  'chat_copy_preview',
                  'Копировать из предпросмотра',
                  icon = icon('angles-left'),
                  width = '100%',
                  style = 'margin-top:6px;'
                ),
                actionButton(
                  'chat_default_promts',
                  'Вернуть по умолчанию',
                  icon = icon('arrow-rotate-left'),
                  class = 'btn-warning',
                  width = '100%',
                  style = 'margin-top:6px;'
                )
              ),
              
              ##### Предпросмотр ----
              column(
                6,
                h4('Предпросмотр инструкций'),
                h5('Системная инструкция:'),
                verbatimTextOutput('system_prompt_preview', placeholder = TRUE),
                h5('Пользовательская инструкция:'),
                verbatimTextOutput('user_prompt_preview', placeholder = TRUE)
              )
            ),
            
            tags$br(),
            ##### Экспорт и импорт ----
            fluidRow(
              column(
                6,
                downloadButton(
                  'download_prompts',
                  'Экспортировать инструкции',
                  class = 'btn-success',
                  style = 'margin-top:25px;'
                )
              ),
              column(
                6,
                fileInput(
                  'upload_prompts',
                  'Импортировать инструкции',
                  accept = c('.RData'),
                  buttonLabel = 'Обзор…',
                  placeholder = 'Файл не выбран'
                )
              )
            )
          )
        )
      ),
      ### История оценок ----
      tabItem(
        tabName = 'history',
        h2('История оценок'),
        tags$button(
          tagList(fontawesome::fa('download'), 'Сохранить CSV'),
          onclick = "Reactable.downloadDataCSV(
            'history-table', 'history.csv',
            {headers: true, sep: ';', dec: ','}
          )",
          class = '
            btn btn-default shiny-download-link btn-success shiny-bound-output',
          style = 'margin-bottom: 20px;'
        ),
        reactableOutput('history')
      )
    )
  )
)
