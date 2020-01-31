source("./utils/get-random-data.R",encoding = "UTF-8", chdir=TRUE)

demandPredictionUI <- function(id, tabName) {
  ns = NS(id)
  DF_PROD_TABLE = generate.df.prod.table()
  prod.table<-setNames(as.list(DF_PROD_TABLE$prod_num), DF_PROD_TABLE$prod_name)
  tabItem(
    tabName = tabName,
    fluidRow(column(width = 12, h2("Product demands"))),
    fluidRow(
      box(width=8,
          title = "상품별 판매 추이",
          solidHeader = FALSE,
          status = "primary",
          align = "middle",
          plotOutput(ns("demand.plot"), height = "700px")),
      column(width=4,
             selectInput(ns("demand.plot.input"),
                         label = "상품: ",
                         selected = 38,
                         choices = prod.table,
                         selectize=TRUE,
                         width = "100%"),
             infoBoxOutput(ns("demand.this.month"),width=12),
             infoBoxOutput(ns("demand.next.month"),width=12))
    )
  )
}



demandPrediction <- function(input, output, session) {
  DF_PRED <- generate.df.pred()
  prod_nums = DF_PRED$prod_num %>% unique
  demand.plots = list()
  for (i in prod_nums) {
    filtered_df = DF_PRED %>% filter(prod_num == i)
    demand.plots[[i]] <-  ggplot(filtered_df[-NROW(filtered_df),], 
                                 aes(x = date, y = 수량))+
      geom_line(color = "cornflowerblue", size = 2) +
      geom_line(data = filtered_df %>% tail(2) %>% mutate(date2 = as.Date(date)), 
                 mapping=aes(x = date2 , y = 수량), 
                linetype = "dashed", size=2,
                color = "cornflowerblue") +
      xlab("") +
      ylab("판매량") + 
      scale_x_date(date_labels = "%Y/%m") + 
      simple_background +
      theme(text = element_text(size=20))
  }
  output[["demand.plot"]] <- renderPlot({
    demand.plots[[as.numeric(input$demand.plot.input)]]
  })
  output[["demand.this.month"]] <- renderInfoBox({
    this.month <- DF_PRED %>% 
      filter((year == 2005) & (month == 12) & (prod_num == as.numeric(input$demand.plot.input))) %>%
      select(수량) %>% 
      unlist
    infoBox(title="이번 달 판매량",value=this.month,color = "blue")
  })
  output[["demand.next.month"]] <- renderInfoBox({
    next.month <- DF_PRED %>% 
      filter((year == 2006) & (month == 1) & (prod_num == as.numeric(input$demand.plot.input))) %>%
      select(수량) %>% 
      unlist
    infoBox(title="다음 달 예측 판매량",value=next.month, color = "blue")
  })
  
}