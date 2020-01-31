clusterComparisonUI <- function(id, tabName) {
  ns = NS(id)
  column.table <- list(
    "총 구매금액" = 1,
    '건당 구매금액' = 2,
    '적립금 사용 비율' = 3,
    '회원 유형' = 4
  )
  tabItem(
    tabName = tabName,
    fluidRow(column(width = 12, h2("Cluster Comparison"))),
    fluidRow(
      box(width=9,
          title = "",
          solidHeader = FALSE,
          status = "primary",
          align = "middle",
          plotOutput(ns("cluster.comparison.plot"), height = "700px")),
      column(width=3,
             selectInput(ns("demand.plot.input"),
                         label = "상품: ",
                         selected = 38,
                         choices = column.table,
                         selectize=TRUE,
                         width = "100%"),
             infoBoxOutput(ns("cluster.info1"),width=12),
             infoBoxOutput(ns("cluster.info2"),width=12),
             infoBoxOutput(ns("cluster.info3"),width=12),
             infoBoxOutput(ns("cluster.info4"),width=12))
    )
  )
}





clusterComparison <- function(input, output, session) {
  plots = list()
  plots[[1]] <- ggplot(DF_CLUSTER %>% mutate(총구매금액=총구매금액/10000,clusterf = as.factor(cluster+1)),
                       aes(x=총구매금액, fill=clusterf)) + 
    geom_density(alpha=0.25) +
    xlab("총구매금액(만 원)") +
    ylab("비율")
  
  plots[[2]] <- ggplot( DF_TOTAL ,aes(x= 주문액, fill= 고객군 )) + 
    geom_density(alpha=0.25) +
    xlab("건당 구매금액") +
    ylab("비율") +
    facet_wrap(~고객군, nrow=2) +
    simple_background
  
  plots[[3]] <- ggplot(DF_CLUSTER, aes(x = 적립금_사용_거래_비율, fill= 고객군 )) +
    geom_histogram(alpha=0.25) +
    xlab("적립금 사용 비율") + 
    ylab("Frequency") +
    facet_wrap(~고객군, nrow=2) +
    simple_background
    
  plots[[4]] <- ggplot(DF_CLUSTER, aes(fill=회원구분_x,y=적립금_사용_거래_비율, x=고객군)) + 
    geom_bar(position="fill", stat="identity",width = .4) +
    ylab("비율") +
    labs(fill = "회원 유형") +
    simple_background
  
    
  
  total.payment.mean.cmp = list()
  single.payment.mean.cmp = list()
  for (i in 1:4) {
    total.payment.mean.cmp[[i]] <- paste0(floor(mean(DF_CLUSTER$총사용적립금[DF_CLUSTER$cluster == (i-1)])), " 원")
    single.payment.mean.cmp[[i]] <- paste0(floor(mean(DF_TOTAL$주문액[DF_TOTAL$cluster == (i-1)])), " 원")
  }
  
  output$cluster.comparison.plot <- renderPlot({
    if (input$demand.plot.input == 1) {
      return(plots[[1]])
    }
    else if (input$demand.plot.input == 2) {
      return(plots[[2]])
    }
    else if (input$demand.plot.input == 3) {
      return(plots[[3]])
    }
    else if (input$demand.plot.input == 4) {
      return(plots[[4]])
    }
  })
  output[[paste0("cluster.info1")]] <- renderInfoBox({
    i = 1
    if (input$demand.plot.input == 1) {
      return(infoBox(
        paste0("고객군 ",i," 평균 총구매금액"),
        total.payment.mean.cmp[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 2) {
      return(infoBox(
        paste0("고객군 ",i," 건당 구매금액"),
        single.payment.mean.cmp[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 3) {
      return(infoBox(
        paste0("고객군 ",i," 적립금 사용 비율"),
        milege.ratio.mean[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 4) {
      return(infoBox(
        paste0("고객군 ",i," 사업자 비율"),
        business.client.ratio[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    
  })
  output[[paste0("cluster.info2")]] <- renderInfoBox({
    i=2
    if (input$demand.plot.input == 1) {
      return(infoBox(
        paste0("고객군 ",2," 평균 총구매금액"),
        total.payment.mean.cmp[[2]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 2) {
      return(infoBox(
        paste0("고객군 ",i," 건당 구매금액"),
        single.payment.mean.cmp[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 3) {
      return(infoBox(
        paste0("고객군 ",i," 적립금 사용 비율"),
        milege.ratio.mean[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 4) {
      return(infoBox(
        paste0("고객군 ",i," 사업자 비율"),
        business.client.ratio[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    
  })
  output[[paste0("cluster.info3")]] <- renderInfoBox({
    i=3
    if (input$demand.plot.input == 1) {
      return(infoBox(
        paste0("고객군 ",3," 평균 총구매금액"),
        total.payment.mean.cmp[[3]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 2) {
      return(infoBox(
        paste0("고객군 ",i," 건당 구매금액"),
        single.payment.mean.cmp[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 3) {
      return(infoBox(
        paste0("고객군 ",i," 적립금 사용 비율"),
        milege.ratio.mean[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 4) {
      return(infoBox(
        paste0("고객군 ",i," 사업자 비율"),
        business.client.ratio[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    
    
  })
  output[[paste0("cluster.info4")]] <- renderInfoBox({
    i=4
    if (input$demand.plot.input == 1) {
      return(infoBox(
        paste0("고객군 ",4," 평균 총구매금액"),
        total.payment.mean.cmp[[4]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 2) {
      return(infoBox(
        paste0("고객군 ",i," 건당 구매금액"),
        single.payment.mean.cmp[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 3) {
      return(infoBox(
        paste0("고객군 ",i," 적립금 사용 비율"),
        milege.ratio.mean[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    else if (input$demand.plot.input == 4) {
      return(infoBox(
        paste0("고객군 ",i," 사업자 비율"),
        business.client.ratio[[i]],
        icon = icon("tags", lib="glyphicon"),
        color = "light-blue"
      ))
    }
    
    
  })
}