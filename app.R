source('global.R')
source('read_data.R', encoding = 'UTF-8', local = TRUE)

# Login Lage
ui_login = tagList(
  div(id = "login",
      wellPanel(print('이 페이지는 직원 전용 입니다.'),
                br(),
                br(),
                textInput("userName", "Username"),
                passwordInput("passwd", "Password"),
                br(),actionButton("Login", "Log in", width = '100%', class = "btn-warning"))),
  tags$style(type="text/css", "#login {font-size:15px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
)

ui_advisor = tagList(
  tabPanel("adret",
           plotlyOutput('adretgraph') %>% withSpinner(),
           br(),
           dataTableOutput('adrettable')
           )
)

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = 'Boolio'),
  
  dashboardSidebar(
    
    tags$head(tags$style(HTML('
        a[href="#shiny-tab-widgets"] {
          z-index: -99999;
        }
        a[href="#"] {
          z-index: -99999;
        }
      '))),
    
    sidebarMenu(
      menuItem("불리오 펀드", icon = icon('chart-line'), startExpanded = TRUE,
               menuSubItem("펀드 누적 수익률", tabName = "cumret"),
               menuSubItem("설정액", tabName = "aum"),
               menuSubItem("펀드 성과", tabName = "stat"),
               menuSubItem("월간 수익률", tabName = "calendar"),
               menuSubItem("일간 기준가", tabName = "funddaily"),
               menuSubItem("펀드 소개영상", tabName = "youtube")
               
      ),
      menuItem("불리오 자문", icon = icon('dashboard'), startExpanded = TRUE,
               menuSubItem("자문 누적 수익률", tabName = "adret"),
               menuSubItem("통계 지표", tabName = "datestat")
               ),
      
      br(),
      br(),
      
      actionButton('how_invest', '불리오 가입 방법',
                   icon = icon("info"), width = '80%', class = "btn-warning"),
      actionButton('fund_define', '불리오 펀드 기준',
                   icon = icon("table"), width = '80%'),
      actionButton('ad_define', '불리오 자문 기준',
                   icon = icon("table"), width = '80%'),
      
      
      br(),
      br(),
      
      tags$div(class="f_fixed",
               a(href="http://pf.kakao.com/_Dxfgxad/chat",
                 target="_blank",
                 img(src = 'kakao.png',
                     style="width: 150px; display: block; margin-left: auto; margin-right: auto;" )
               )
      )
    )
  ),
  
  dashboardBody(
    
    meta() %>%
      meta_social(
        title = "불리오 펀드 수익률 대시보드",
        url = "https://doomoolmori.shinyapps.io/boolio",
        image = "https://github.com/hyunyulhenry/boolio/blob/master/www/boolio.png?raw=true"
      ),
    
    bsModal("bs_how_invest", "불리오 가입 방법",
            "how_invest", size = "medium",
            includeMarkdown('inv.Rmd')
    ),
    
    bsModal("bs_fund_define", "불리오 펀드 기준",
            "fund_define", size = "medium",
            includeMarkdown('fund_define.Rmd')
    ),
    
    bsModal("bs_ad_define", "불리오 자문 기준",
            "ad_define", size = "medium",
            uiOutput('markdown')
    ),
    
    tabItems(
      
      tabItem("cumret",
              plotlyOutput('fundretgraph') %>% withSpinner(),
              br(),
              dataTableOutput('fundrettable')
      ),
      tabItem("aum", plotlyOutput('aum') %>% withSpinner(),
              br(),
              dataTableOutput('aum_table')
      ),
      tabItem("stat", 
              uiOutput('statcategory'),
              br(),
              dataTableOutput('fund_stat') %>% withSpinner()
      ),
      tabItem("calendar", 
              tags$head(
                tags$style(HTML("hr {border-top: 1px solid #000000;}"))
              ),
              print('글로벌EMP H형(원화투자)'),
              dataTableOutput('calenderhedge') %>% withSpinner(),
              br(),
              hr(),
              br(),
              print('글로벌EMP UH형(달러투자)'),
              dataTableOutput('calenderunhedge'),
              br(),
              hr(),
              print('단위: %')
      ),
      tabItem("funddaily",
              uiOutput('navret') %>% withSpinner(),
              br(),
              dataTableOutput('daily')
      ),
      tabItem("youtube",
              htmlOutput("video")
      ),
      tabItem("adret",
              uiOutput('page')
      ),
      tabItem("datestat",
              uiOutput('date'),
              br(),
              dataTableOutput('returnstat') %>% withSpinner()
      )
      
  )
  )
)

server <- shinyServer(function(input, output, session) {
  
  source('read_ui.R', local = TRUE, encoding = 'UTF-8')
  source('login.R', local = TRUE, encoding = 'UTF-8')
  
  # Login
  Logged = FALSE;
  my_username <- my_username
  my_password <- my_password
  
  USER <- reactiveValues(Logged = Logged)
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  output$page <- renderUI({
    if (USER$Logged){
      return({  div(class="outer", do.call(bootstrapPage,c("",ui_advisor)))})
    } else {
      return({  div(class="outer", do.call(bootstrapPage,c("",ui_login)))})
    }
  })
  
  
  
  # 펀드 누적 수익률
  fund_cum = 
    df_raw %>% select(DATE, contains('불리오')) %>%
    mutate(`불리오EMP H Ae` = `불리오EMP H Ae` / `불리오EMP H Ae`[1L] - 1) %>%
    mutate(`불리오EMP UH Ae` = `불리오EMP UH Ae` / `불리오EMP UH Ae`[1L] - 1) %>%
    rename('글로벌EMP H형(원화투자)' = '불리오EMP H Ae',
           '글로벌EMP UH형(미달러투자)' = '불리오EMP UH Ae')  %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
  
  # 펀드 누적 수익률 그래프
  output$fundretgraph = renderPlotly({
  (fund_cum %>%
    pivot_longer(names_to  = 'key', values_to = 'value', -DATE) %>%
      ggplot(aes(x = DATE, y = value)) +
      geom_line(aes(color = key)) +
      theme_light() +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      scale_x_date(date_breaks  = '3 months',
                   labels = scales::date_format("%Y-%m")) +
      theme(legend.position = 'top', legend.title=element_blank(),
            panel.grid.major = element_line(color = '#f8f8f8'),
            axis.text.x = element_text(angle = 30)) +
      scale_color_manual(values=c("#fcbf5c", "#ff8935"))
  ) %>% ggplotly() %>%
    layout(legend = list(
      orientation = "h",
      y = 1.15
    ))
  })
  
  output$fundrettable = renderDataTable({
    fund_cum %>%
      datatable(rownames = FALSE, extensions = 'Buttons',
                options = list(dom = 'tB',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength = 1000000,
                               scrollY = "300px")) %>%
      formatPercentage(2:3, digits=2)
      
  })
  
  # AUM 그래프
  output$aum = renderPlotly({
    
    (read_aum() %>%
      pivot_longer(names_to  = 'key', values_to = 'value', -DATE) %>%
      ggplot(aes(x = DATE, y = value)) +
      geom_line(aes(color = key)) +
      theme_light() +
      xlab(NULL) +
      ylab(NULL) +
      scale_x_date(labels = scales::date_format("%y-%m")) +
      theme(legend.position = 'top', legend.title=element_blank())
    ) %>% ggplotly()
    
  })
  
  # AUM 테이블
  output$aum_table = renderDataTable({
    
    read_aum() %>%
      mutate('설정/해지' = AUM - lag(AUM)) %>%
      mutate(`설정/해지` = round(`설정/해지`, 4)) %>%
      select(DATE, AUM, `설정/해지`, NAV) %>%
      
      datatable(rownames = FALSE, extensions = 'Buttons',
                options = list(dom = 'tB',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength = 1000000,
                               scrollY = "300px")) %>%
                  formatStyle(3, color = styleInterval(0, c('blue', 'red')))
  })
  
  # 월간 수익률(H)
  output$calenderhedge = renderDataTable({
    df_xts$`불리오EMP H Ae` %>%
      set_colnames('Returns') %>%
      apply.monthly(., Return.cumulative) %>%
      table.CalendarReturns(digits =2, geometric = TRUE) %>%
      rownames_to_column(var = 'Year') %>%
      dplyr::rename('YTD' = 'Returns') %>%
      datatable(rownames = FALSE, 
                options = list(dom = 't',
                               pageLength = 1000000
                ) 
      ) %>%
      formatStyle(columns = c("YTD"), fontWeight = 'bold')
  })
  
  # 월간 수익률(UH)
  output$calenderunhedge = renderDataTable({
    df_xts$`불리오EMP UH Ae` %>%
      set_colnames('Returns') %>%
      apply.monthly(., Return.cumulative) %>%
      table.CalendarReturns(digits =2, geometric = TRUE) %>%
      rownames_to_column(var = 'Year') %>%
      dplyr::rename('YTD' = 'Returns') %>%
      datatable(rownames = FALSE, 
                options = list(dom = 't',
                               pageLength = 1000000
                ) 
      ) %>%
      formatStyle(columns = c("YTD"), fontWeight = 'bold')
  })
  
  # 기준가 
  output$daily = renderDataTable({
    
    req(input$navret)

    dailynav = if(input$navret == '기준가') {
      df_raw %>% select(DATE, contains('불리오'))
    } else {
      df %>% select(DATE, contains('불리오')) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4)))
    }
    
    dailynav %>%
      datatable(rownames = FALSE, extensions = 'Buttons',
                options = list(dom = 'tB',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength = 1000000,
                               scrollY = "600px")
      )
    
  })
  
  # 유튜브
  output$video <- renderUI({
    tags$iframe(src = "https://www.youtube.com/embed/XLbszY5qX9g", width = 1020, height = 630)
  })
  
  # 불리오 자문 수익률
  ad_cum = 
    boolio_return_bind %>%
    fortify.zoo(names = 'DATE') %>%
    select(-contains('불리오')) %>%
    mutate(across(where(is.numeric), ~ cumprod(1 + .x)) - 1) %>%
    mutate(across(where(is.numeric), ~ round(.x, 4)))
  
  output$adretgraph = renderPlotly({
    (ad_cum %>%
      pivot_longer(names_to  = 'key', values_to = 'value', -DATE) %>%
      mutate(key = factor(key, levels = c('매운맛', '오리지널', '순한맛'))) %>%
      ggplot(aes(x = DATE, y = value)) +
      geom_line(aes(color = key)) +
      theme_light() +
      xlab(NULL) +
      ylab(NULL) +
      scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
      scale_color_manual(values=c("#ea4221", "#fbbd04", "#4286f4")) +
      scale_x_date(date_breaks  = '1 months',
                   labels = scales::date_format("%Y-%m")) +
      theme(legend.position = 'top', legend.title=element_blank(),
            axis.text.x = element_text(angle = 30))
    ) %>% ggplotly() %>%
      layout(legend = list(
        orientation = "h",
        y = 1.15
      ))
  })
  
  output$adrettable = renderDataTable({
    ad_cum %>%
      datatable(rownames = FALSE, extensions = 'Buttons',
                options = list(dom = 'tB',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength = 1000000,
                               scrollY = "300px")) %>%
      formatPercentage(2:4, digits=2)
    
  })
  
  output$markdown = renderUI({
    includeHTML('ad_define.html')
  })
  
  output[["fund_stat"]] <- renderDT({
    
    req(input$statcategory)
    
    data = if(input$statcategory == 'H') {
      stat_hedge()
    } else {
      stat_unhedge()
    }
    
    dtable = data %>%
      datatable(rownames = FALSE, 
                options = list(dom = 't',
                               rownames = FALSE,
                               rowsGroup = list(0),
                               pageLength = 1000000)) 
    
    path <- glue(getwd(), '/www')
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    
    dtable$dependencies <- c(dtable$dependencies, list(dep))
    dtable
  })
  
  output$returnstat = renderDataTable({
    
    req(input$date[[1]])
    
    t1 = input$date[[1]] %>% as.character()
    t2 = input$date[[2]] %>% as.character()
    
    df_cut = boolio_return_bind[glue('{t1}::{t2}')]
    
    res = rbind(
      Return.cumulative(df_cut),
      Return.annualized(df_cut),
      StdDev.annualized(df_cut),
      maxDrawdown(df_cut),
      UpsideFrequency(df_cut),
      SharpeRatio.annualized(df_cut),
      SortinoRatio(df_cut),
      CalmarRatio(df_cut)
    ) 
    
    res %>%
      as_tibble() %>% 
      mutate('기준' = c('누적 수익률(%)', '연율화 수익률(%)', '연율화 변동성(%)',
                      'MDD(%)', '승률(%)', '연율화 샤프지수','소티노 지수', '칼마 지수')) %>%
      select('기준', everything()) %>%
      mutate(across(where(is.numeric), 
                    ~ if_else(기준 %in% c('연율화 샤프지수', '소티노 지수', '칼마 지수'), 
                                round(.x, 4), round(.x * 100, 2)))) %>%
      datatable(rownames = FALSE, 
                options = list(dom = 't',
                               rownames = FALSE)) 
    
  })
  
})

shinyApp(ui = ui, server)

# rsconnect::deployApp()
