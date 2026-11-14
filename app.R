library(shiny)
library(bslib)
library(shinyWidgets)
source("engine.R")

ui <- page_navbar(
  title = "Shoe Search test",
  fillable = FALSE,
  nav_panel("Shoe Search",
            layout_columns(
              col_widths = c(6, 6),
              
              # ── 왼쪽: 내가 넣은 것 ──
              card(
                card_header("Your photo", style = "font-size: 20px; font-weight: 700;"),
                radioGroupButtons("src", span("STEP1: How would you like to choose a photo?",
                                              style = "font-size: 18px; font-weight: 600;"),
                                  choices = c("Catalogue", "Self-check", "Upload"),
                                  selected = character(0), justified = TRUE),
                conditionalPanel("input.src == 'Catalogue'",
                                 numericInput("cat_id", "Type a catalogue id", value = 1, min = 1, max = 2000)),
                conditionalPanel("input.src == 'Self-check'",
                                 selectInput("sc_file", "Select a self-check photo", sprintf("selfcheck_%02d.png", 1:4))),
                conditionalPanel("input.src == 'Upload'",
                                 fileInput("photo", "Upload a photo", accept = c(".jpg", ".jpeg", ".png"))),
                numericInput("K", span("STEP2: Choose the number of PCs (K)",
                                       style = "font-size: 18px; font-weight: 600;"), value = 100, min = 0, max = 2000, step = 50),
                sliderInput("K_slider", NULL, min = 0, max = 2000, value = 100),
                layout_columns(
                  col_widths = c(6, 6),
                  div(h5("Your Photo"), plotOutput("ss_query", width = "100%", height = "auto")),
                  div(h5("Rebuilt with K PCs"), plotOutput("ss_rebuild", width = "100%", height = "auto"))
                )
              ),
              
              # ── 오른쪽: 찾아낸 것 ──
              card(
                card_header("Search result", style = "font-size: 20px; font-weight: 700;"),
                h5("Top 5"),
                tableOutput("ss_top5"),
                uiOutput("ss_pred"),
                layout_columns(
                  col_widths = c(6, 6),
                  div(h5("Closest catalogue photo"), plotOutput("ss_best", width = "100%", height = "auto")),
                  div(h5("Average shoe"),            plotOutput("ss_mean", width = "100%", height = "auto"))
                )
              )
            )
  )
)



server <- function(input, output, session) {
  # [1] Best Match
  # 복원: 사용자가 K_slider값을 바꿨을 때 K를 씽크해주기
  observeEvent(input$K_slider, {
    if (!identical(input$K, input$K_slider)) {
      updateNumericInput(session, "K", value = input$K_slider)
    }
  })
  
  # 복원: 사용자가 K값을 바꿨을 때 K_slider를 씽크해주기
  observeEvent(input$K, {
    if (!identical(input$K, input$K_slider)) {
      updateSliderInput(session, "K_slider", value = input$K)
    }
  })
  
  # 선택된 사진의 정보 모으기
  query <- reactive({
    req(input$src)
    # case1. 카탈로그 사진
    if (input$src == "Catalogue") {
      id <- input$cat_id
      validate(need(!is.na(id) && id >= 1 && id <= 2000,
                    "Catalogue id는 1부터 2000 사이로 입력해주세요"))
      list(x = read_shoe(sprintf("shoes/%04d.png", id)),
           zq = pca$x[id, ],
           exclude = id,
           answer = meta$category[id])
    }
    # case2. self-check 사진
    else if (input$src == "Self-check") {
      path <- file.path("selfcheck", input$sc_file)
      list(x = read_shoe(path),
           zq = photo_score(path),
           exclude = NULL,
           answer = selfcheck_labels$category[selfcheck_labels$photo == input$sc_file])
    }
    # case3. 업로드한 사진
    else {
      req(input$photo)
      path <- input$photo$datapath
      list(x = read_shoe(path),
           zq = photo_score(path),
           exclude = NULL,
           answer = NA)
    }
  })
  
  output$ss_query <- renderPlot({
    show_shoe(query()$x)
  }, height = function() session$clientData$output_ss_query_width)
  output$ss_rebuild <- renderPlot({
    validate(need(!is.na(input$K) && input$K >= 1 && input$K <= 2000,
                  "K는 1부터 2000 사이로 입력해주세요"))
    show_shoe(rebuild(query()$zq, input$K))
  },height = function() session$clientData$output_ss_rebuild_width)
  
  #
  res <- reactive({
    validate(need(!is.na(input$K) && input$K >= 1 && input$K <= 2000,
                  "K는 1부터 2000 사이로 입력해주세요"))
    search_shoe(query()$zq, input$K, exclude_id = query()$exclude)
  })
  
  output$ss_mean <- renderPlot({
    show_shoe(pca$center)
  }, height = function() session$clientData$output_ss_mean_width)
  
  # [2] Top5
  output$ss_top5 <- renderTable({
    data.frame(
      Rank = 1:5,
      id = res()$top5,
      Category = res()$category,
      Distance = res()$dist
    )
  })
  
  output$ss_best <- renderPlot({
    show_shoe(read_shoe(sprintf("shoes/%04d.png", res()$top5[1])))
  },height = function() session$clientData$output_ss_best_width)
  
  # [3] Prediction
  output$ss_pred <- renderUI({
    if (is.na(query()$answer)) {
      pred <- res()$pred
      msg <- paste("Predicted category of the shoe: ", pred)
      color <- "#1A4FA0"
      bg <- "#EAF2FF"
    }
    else {
      pred <- res()$pred
      ans <- query()$answer
      line1 <- paste("Predicted category of the shoe: ", pred)
      line2 <- paste("And, the actual category: ", ans)
      if (pred == ans) {
        line3 <- "So the prediction is CORRECT!"
        color <- "#1E7B34"
        bg <- "#E8F5EC"
      }
      else {
        line3 <- "So the prediction is WRONG!"
        color <- "#B42318"
        bg <- "#FDECEA"
      }
      msg <- paste(line1, line2, line3, sep="<br>")
    }
    div(HTML(msg),
        style = paste0("font-size: 20px; font-weight: 600; padding: 12px 16px; border-radius: 8px;",
                       "color: ", color, "; background-color: ", bg, "; border: 2px solid ", color, ";"))
  })
  
}


shinyApp(ui, server)
