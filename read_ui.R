output$date = renderUI({
  dateRangeInput(
    inputId = 'date',
    label = '기간을 선택하세요',
    start = head(df$DATE, 1),
    end = tail(df$DATE, 1),
    min = head(df$DATE, 1),
    max = tail(df$DATE, 1),
    format = "yyyy-mm-dd",
    separator = " - ")
})

# output$period = renderUI({
#   awesomeRadio(
#     inputId = "period",
#     label = "리밸런싱 주기", 
#     choices = c("months", "quarters", "years"),
#     selected = "months",
#     inline = FALSE, 
#     checkbox = TRUE
#   )
# })

# output$download = renderUI({
#   downloadBttn(
#     outputId = "asset",
#     label = '데이터 다운로드',
#     style = "bordered",
#     color = "primary",
#     size = 'xs'
#   )
# })

output$navret = renderUI({
  awesomeRadio(
    inputId = "navret",
    label = "",
    choices = c("기준가", "수익률"),
    selected = "기준가",
    checkbox = TRUE
  )
})

output$statcategory = renderUI({
  awesomeRadio(
    inputId = "statcategory",
    label = "펀드 종류를 선택하세요",
    choices = c("H", "UH"),
    selected = "H",
    checkbox = TRUE
  )
})