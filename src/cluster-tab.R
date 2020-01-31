# ---------------------------------------------------
# UI function of cluster tab
# ---------------------------------------------------
clusterTabUI <- function(id, i) {
  ns = NS(id)
  tabItem(
    tabName = paste0("cluster", i),
    fluidRow(column(width = 12, h2("Customers"))),
    fluidRow(
      box(width=8,
          title = "주별 매출",
          solidHeader = FALSE,
          status = "primary",
          align = "middle",
          plotOutput(ns("revenue.plot"), height = "400px")),
      column(width=4,
             radioGroupButtons(
               inputId= ns("revenue.plot.input"), 
               label = "X 축:", 
               choices = c("주간", "월별"),
               selected = "주간", size = "normal",
               justified = TRUE, status = "light",
               checkIcon = list(yes = icon("ok", lib = "glyphicon"))
               ),
             valueBoxOutput(ns("user.count"), width = 12),
             valueBoxOutput(ns("three.month.revenue"), width = 12),
             valueBoxOutput(ns("single.3month.revenue"), width = 12)
             )
    ),
    fluidRow(
      box(
        title = "재개편등급",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "middle",
        plotOutput(ns("new.grade.pie.chart"), height = "400px")
      ),
      box(
        title = "이용기간",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("used.period.hist"), height = "300px"),
        sliderInput(
          inputId = ns("used.period.hist.input"),
          label = 'X 범위',
          value = used.period.hist.input[[i]],
          min = used.period.hist.input[[i]][1],
          max = used.period.hist.input[[i]][2]
        )
      ),
      box(
        title = "최근접속",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("recent.login.hist"), height = "300px"),
        sliderInput(
          inputId = ns("recent.login.hist.input"),
          label = 'X 범위',
          value = recent.login.hist.input[[i]],
          min = recent.login.hist.input[[i]][1],
          max = recent.login.hist.input[[i]][2]
        )
      )
      
    ),
    fluidRow(column(width = 12, h2("구입상품"))),
    fluidRow(
      box(
        title = "가장 많이 팔린 상품",
        width = 5,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        dataTableOutput(ns("most.sold.item.dt"))
      ),
      box(
        title = "연계 상품",
        width = 7,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        collapsible = TRUE,
        imageOutput(ns("cluster.item.img"))
      ),
    ),
    fluidRow(column(width = 12, h2("구매액"))),
    fluidRow(
      box(
        title = "총구매금액",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("total.payment.hist")),
        sliderInput(
          inputId = ns("total.payment.hist.input"),
          label = 'X 범위',
          value = total.payment.hist.input[[i]],
          min = total.payment.hist.input[[i]][1],
          max = total.payment.hist.input[[i]][2]
        )
      ),
      box(
        title = "건당구매금액",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("single.payment.hist")),
        sliderInput(
          inputId = ns("single.payment.hist.input"),
          label = 'X 범위',
          value = single.payment.hist.input[[i]],
          min = single.payment.hist.input[[i]][1],
          max = single.payment.hist.input[[i]][2]
        )
      ),
      box(
        title = "누적주문건수",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("cum.order.count.hist")),
        sliderInput(
          inputId = ns("cum.order.count.hist.input"),
          label = 'X 범위',
          value = cum.order.count.hist.input[[i]],
          min = cum.order.count.hist.input[[i]][1],
          max = cum.order.count.hist.input[[i]][2]
        )
      ),
      infoBoxOutput(ns("total.payment.mean"), width = 4),
      infoBoxOutput(ns("single.payment.mean"), width = 4)
    ),
    fluidRow(column(width = 12, h2(
      "마일리지 사용"
    ))),
    fluidRow(
        box(
          title = "총 사용 적립금",
          width = 4,
          solidHeader = FALSE,
          status = "primary",
          align = "center",
          plotOutput(ns("total.milege.hist")),
          sliderInput(
            inputId = ns("total.milege.hist.input"),
            label = 'X 범위',
            value = total.milege.hist.input[[i]],
            min = total.milege.hist.input[[i]][1],
            max = total.milege.hist.input[[i]][2]
          )
        ),
        box(
          title = "평균 적립금 사용 거래 비율",
          width = 4,
          solidHeader = FALSE,
          status = "primary",
          align = "center",
          plotOutput(ns("milege.ratio.hist")),
          sliderInput(
            inputId = ns("milege.ratio.hist.input"),
            label = 'X 범위',
            value = milege.ratio.hist.input[[i]],
            min = milege.ratio.hist.input[[i]][1],
            max = milege.ratio.hist.input[[i]][2]
          )
        ),
        valueBoxOutput(ns("total.milege.mean"), width = 4),
        valueBoxOutput(ns("milege.ratio.mean"), width = 4)),
    fluidRow(column(width = 12, h2(
      "쿠폰 사용"
    ))),
    fluidRow(
        box(
          title = "쿠폰 할인액",
          width = 4,
          solidHeader = FALSE,
          status = "primary",
          align = "center",
          plotOutput(ns("coupon.amnt.hist")),
          sliderInput(
            inputId = ns("coupon.amnt.hist.input"),
            label = 'X 범위',
            value = coupon.amnt.hist.input[[i]],
            min = coupon.amnt.hist.input[[i]][1],
            max = coupon.amnt.hist.input[[i]][2]
          ) ),
        column(width=4,
               valueBoxOutput(ns("coupon.amnt.mean"), width = 12),
               valueBoxOutput(ns("coupon.use.ratio"), width = 12)
               )
        ),
    fluidRow(column(width = 12, h2(
      "기타"
    ))),
    fluidRow(
      box(
        title = "회원등급",
        width = 4,
        solidHeader = FALSE,
        status = "primary",
        align = "center",
        plotOutput(ns("client.class.pie.chart"), height="300px")
      ),
      column(width=4,
             infoBoxOutput(ns("permenant.client.ratio"), width = 12),
             infoBoxOutput(ns("business.client.ratio"), width = 12))
    )
  )
}


# ---------------------------------------------------
# server function of cluster tab
# ---------------------------------------------------
clusterTab <- function(input, output, session) {
  
  i <- as.numeric(substr(session$ns(""), 4, 4))
  
  output[["cluster.item.img"]] <- renderImage({
    return(list(
      src = CLUSTER_PNG_SRC[i],
      contentType = "image/png",
      alt =  paste0("cluster", i),
      height = 400
    ))
  }, deleteFile = FALSE)
  
  output[["milege.ratio.hist"]] <- renderPlot({
    milege.ratio.hist[[i]] +
      xlim(input[["milege.ratio.hist.input"]][1],
           input[["milege.ratio.hist.input"]][2])
  })
  output[["total.milege.hist"]] <- renderPlot({
    total.milege.hist[[i]] +
      xlim(input[["total.milege.hist.input"]][1],
           input[["total.milege.hist.input"]][2])
  })
  output[["total.milege.ratio.hist"]] <- renderPlot({
    total.milege.hist[[i]] +
      xlim(input[["total.milege.ratio.hist.input"]][1],
           input[["total.milege.ratio.hist.input"]][2])
  })
  output[["total.payment.hist"]] <- renderPlot({
    total.payment.hist[[i]] +
      xlim(input[["total.payment.hist.input"]][1],
           input[["total.payment.hist.input"]][2])
  })
  output[["single.payment.hist"]] <- renderPlot({
    single.payment.hist[[i]] +
      xlim(input[["single.payment.hist.input"]][1],
           input[["single.payment.hist.input"]][2])
  })
  output[["cum.order.count.hist"]] <- renderPlot({
    cum.order.count.hist[[i]] +
      xlim(input[["cum.order.count.hist.input"]][1],
           input[["cum.order.count.hist.input"]][2])
  })
  output[["client.class.pie.chart"]] <- renderPlot({
    client.class.pie.chart[[i]]
  })
  output[["used.period.hist"]] <- renderPlot({
    used.period.hist[[i]] +
      xlim(input[["used.period.hist.input"]][1],
           input[["used.period.hist.input"]][2])
  })
  output[["recent.login.hist"]] <- renderPlot({
    recent.login.hist[[i]] +
      xlim(input[["recent.login.hist.input"]][1],
           input[["recent.login.hist.input"]][2])
  })
  output[["coupon.amnt.hist"]] <- renderPlot({
    coupon.amnt.hist[[i]] +
      xlim(input[["coupon.amnt.hist.input"]][1],
           input[["coupon.amnt.hist.input"]][2])
  })
  output[["new.grade.pie.chart"]] <- renderPlot({
    new.grade.pie.chart[[i]]
  })
  
  output[["most.sold.item.dt"]] <- renderDataTable({
    most.sold.item.dt[[i]]
  })
  output[["permenant.client.ratio"]] <- renderInfoBox({
    color = ifelse(parse_number(permenant.client.ratio[[i]])>75, 
                   "orange" , "light-blue")
    infoBox("평생회원비율",
            permenant.client.ratio[[i]],
             icon = icon("users"),
             color = color)
  })
  output[["coupon.use.ratio"]] <- renderValueBox({
    valueBox(coupon.use.ratio[[i]],
             "쿠폰사용비율",
             icon = icon("ticket-alt"),
             color = "purple")
  })
  output[["milege.ratio.mean"]] <- renderValueBox({
    valueBox(milege.ratio.mean[[i]],
             "평균 적립금 사용 거래 비율",
             icon = icon("coins"),
             color = "purple")
  })
  output[["total.milege.mean"]] <- renderValueBox({
    valueBox(total.milege.mean[[i]],
             "평균 총 사용 적립급",
             icon = icon("coins"),
             color = "purple")
  })
  output[["coupon.amnt.mean"]] <- renderValueBox({
    valueBox(coupon.amnt.mean[[i]],
             "평균 쿠폰 할인액",
             icon = icon("percent"),
             color = "purple")
  })
  output[["total.payment.mean"]] <- renderInfoBox({
    infoBox(
      "평균 총구매금액",
      total.payment.mean[[i]],
      icon = icon("tags", lib="glyphicon"),
      color = "light-blue"
    )
  })
  output[["single.payment.mean"]] <- renderInfoBox({
    infoBox(
      "건당 구매금액",
      single.payment.mean[[i]],
      icon = icon("tag", lib="glyphicon"),
      color = "light-blue"
    )
  })
  output[["business.client.ratio"]] <- renderInfoBox({
    color = ifelse(parse_number(business.client.ratio[[i]])>75, 
                   "orange" , "light-blue")
    infoBox(
      "사업가 고객 비율",
      business.client.ratio[[i]],
      icon = icon("building"),
      color = color
    )
  })
  output[["user.count"]] <- renderValueBox({
    valueBox(user.count[[i]],
             "유저수(비율)",
             icon = icon("users"),
             color = "light-blue")
  })
  output[["three.month.revenue"]] <- renderValueBox({
    valueBox(
      three.month.revenue[[i]],
      "3개월 매출 기여도(비율)",
      icon = icon("won-sign"),
      color = "yellow"
    )
  })
  output[["single.3month.revenue"]] <- renderValueBox({
    valueBox(single.3month.revenue[[i]],
             "인당 3개월 매출",
             icon = icon("coins"),
             color = "olive")
  })
  output[["revenue.plot"]] <- renderPlot({
    if (input$revenue.plot.input == "주간") {
      return(weekly.revenue.hist[[i]])  
    } else {
      return(monthly.revenue.hist[[i]])  
    }
    
  })
}