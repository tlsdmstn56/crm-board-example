install_load <- function (package1, ...)  {
  # convert arguments to vector
  packages <- c(package1, ...)
  # start loop to determine if each package is installed
  for (package in packages) {
    # if package is installed locally, load
    if (package %in% rownames(installed.packages()))
      do.call('library', list(package))
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  }
}

install_load(
  "shiny",
  "shinydashboard",
  "DT",
  "readr",
  "dplyr",
  "ggplot2",
  "shinycssloaders",
  "tidyr",
  "tibble",
  "shinyBS",
  "shinyWidgets",
  "gridExtra",
  "devtools",
  "testit",
  "lubridate",
  "plotly"
)
if (!("dashboardthemes" %in% rownames(installed.packages()))) {
  install_github("nik01010/dashboardthemes")
}
library(dashboardthemes)
source("./utils/get-random-data.R",encoding = "UTF-8", chdir=TRUE)
# -------------------------------------------------
# pre session processing
# -------------------------------------------------
theme_set(theme_grey())
# Data Import
DF_CLUSTER <- generate.df.cluster()
DF_TOTAL <- generate.df.total()
DF_NEW_ITEM_NAME <-generate.df.new.item.name()

DF_CLUSTER = DF_CLUSTER %>% mutate(고객군=as.factor(cluster+1))
DF_TOTAL = DF_TOTAL %>% mutate(고객군=as.factor(cluster+1))
CLUSTER_PNG_SRC = c(
  "../resource/cluster00_20.png",
  "../resource/cluster01_30.png",
  "../resource/cluster10_20.png",
  "../resource/cluster11_20.png"
)

for(filename in CLUSTER_PNG_SRC)
{
  generate.graph.img(filename)
}

simple_background <-
  theme(
    axis.line = element_line(colour = "gray60"),
    axis.text.x = element_text(colour = "gray60"),
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )
simple_background.pie <- theme(
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)

#----------------총구매금액 히스토그램-------------------------
get.total.payment.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  총구매금액)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background +
    xlab('총구매금액(만 원)') +
    scale_x_continuous(
      labels = function(x)
        format(x / 10000)
    )
}
total.payment.hist = list()
total.payment.hist.input = list()
for (i in 1:4) {
  total.payment.hist[[i]] = get.total.payment.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "총구매금액"]
  total.payment.hist.input[[i]] = c(a %>% min, a %>% max)
}
#----------------총구매금액 평균-------------------------
get.total.payment.mean <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$총구매금액[DF_CLUSTER$cluster == cluster]) /
                10000, 1), " 만원")
}
total.payment.mean = list()
for (i in 1:4) {
  total.payment.mean[[i]] = get.total.payment.mean(i)
}

#----------------건당 구매금액 히스토그램-------------------------
get.single.payment.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_TOTAL[DF_TOTAL$cluster == cluster,] %>% mutate(tmp = 주문액 / 10000),
         aes(x = tmp)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background +
    xlab('건당 구매금액(만 원)')
}
single.payment.hist = list()
single.payment.hist.input = list()
for (i in 1:4) {
  single.payment.hist[[i]] = get.single.payment.hist(i)
  a = DF_TOTAL[DF_TOTAL$cluster == (i - 1),] %>% mutate(tmp = 주문액 / 10000) %>% select(tmp)
  single.payment.hist.input[[i]] = c(a %>% min, a %>% max)
}
#----------------건당 구매금액 평균-------------------------
get.single.payment.mean <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_TOTAL$주문액[DF_TOTAL$cluster == cluster]) / 10000, 1), " 만원")
}
single.payment.mean = list()
for (i in 1:4) {
  single.payment.mean[[i]] = get.single.payment.mean(i)
}

#----------------사용적립금 비율 히스토그램-------------------------
get.milege.ratio.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  적립금_사용_거래_비율)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
milege.ratio.hist = list()
milege.ratio.hist.input = list()
for (i in 1:4) {
  milege.ratio.hist[[i]] = get.milege.ratio.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "적립금_사용_거래_비율"]
  milege.ratio.hist.input[[i]] = c(a %>% min, a %>% max)
}
#----------------사용적립금 비율 평균-------------------------
get.milege.ratio.mean <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$적립금_사용_거래_비율[DF_CLUSTER$cluster == cluster]) *
                100), "%")
}
milege.ratio.mean = list()
for (i in 1:4) {
  milege.ratio.mean[[i]] = get.milege.ratio.mean(i)
}
#----------------총사용적립금 히스토그램--------------------------
get.total.milege.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  총사용적립금)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
total.milege.hist = list()
total.milege.hist.input = list()
for (i in 1:4) {
  total.milege.hist[[i]] = get.total.milege.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "총사용적립금"]
  total.milege.hist.input[[i]] = c(a %>% min, a %>% max)
}
#----------------총사용적립금 평균-------------------------
get.total.milege.mean <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$총사용적립금[DF_CLUSTER$cluster == cluster]) /
                10000, 1), " 만원")
}
total.milege.mean = list()
for (i in 1:4) {
  total.milege.mean[[i]] = get.total.milege.mean(i)
}
#----------------평생회원비율율--------------------------
get.permenant.client.ratio <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$평생회원[DF_CLUSTER$cluster == cluster]) * 100), "%")
}
permenant.client.ratio = list()
for (i in 1:4) {
  permenant.client.ratio[[i]] = get.permenant.client.ratio(i)
}

#----------------사업자 비율율비율율--------------------------
get.business.client.ratio <- function(cluster1) {
  cluster1 = cluster1 - 1
  C = DF_CLUSTER %>% filter(회원구분_x == "사업자" &
                                   cluster == cluster1) %>% count
  L = DF_CLUSTER %>% filter(cluster == cluster1) %>% NROW
  paste(round(C / L * 100), "%")
}
business.client.ratio = list()
for (i in 1:4) {
  business.client.ratio[[i]] = get.business.client.ratio(i)
}

#----------------누적 주문건수 히스토그램--------------------------
get.cum.order.count.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  누적주문건수)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
cum.order.count.hist = list()
cum.order.count.hist.input = list()
for (i in 1:4) {
  cum.order.count.hist[[i]] = get.cum.order.count.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "누적주문건수"]
  cum.order.count.hist.input[[i]] = c(a %>% min, a %>% max)
}

#--------------- 재개편 등급 파이차트--------------------------
get.new.grade.pie.chart <- function(cluster_) {
  cluster_ = cluster_ - 1
  table = DF_CLUSTER %>%
    filter(cluster == cluster_) %>%
    group_by(재개편등급) %>%
    summarize(n = n()) %>%
    mutate(n = n / sum(n) * 100) %>%
    mutate(ypos = cumsum(n) - 0.5 * n)
  # table$n = table$n / (table$n%>%sum)*100
  ggplot(table, aes(x = "", y = n, fill =  재개편등급)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    scale_fill_brewer(palette = "Blues") +
    # theme_void() +
    theme(legend.position = "bottom") +
    simple_background.pie +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}
new.grade.pie.chart = list()
for (i in 1:4) {
  new.grade.pie.chart[[i]] = get.new.grade.pie.chart(i)
}

#--------------- 회원등급 파이차트--------------------------
get.client.class.pie.chart <- function(cluster) {
  cluster = cluster - 1
  table = DF_CLUSTER %>%
    filter(cluster == cluster) %>%
    group_by(회원등급) %>%
    summarize(n = n()) %>%
    mutate(n = n / sum(n) * 100) %>%
    mutate(ypos = cumsum(n) - 0.5 * n)
  # table$n = table$n / (table$n%>%sum)*100
  ggplot(table, aes(x = "", y = n, fill =  회원등급)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y") +
    scale_fill_brewer(palette = "Blues") +
    # theme_void() +
    theme(legend.position = "bottom") +
    simple_background.pie +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}
client.class.pie.chart = list()
for (i in 1:4) {
  client.class.pie.chart[[i]] = get.client.class.pie.chart(i)
}

#----------------이용기간 히스토그램-------------------------
get.used.period.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  이용기간)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
used.period.hist = list()
used.period.hist.input = list()
for (i in 1:4) {
  used.period.hist[[i]] = get.used.period.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "이용기간"]
  used.period.hist.input[[i]] = c(a %>% min, a %>% max)
}

#----------------최근접속 히스토그램-------------------------
get.recent.login.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  최근접속)) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
recent.login.hist = list()
recent.login.hist.input = list()
for (i in 1:4) {
  recent.login.hist[[i]] = get.recent.login.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "최근접속"]
  recent.login.hist.input[[i]] = c(a %>% min, a %>% max)
}

#----------------쿠폰사용액 히스토그램-------------------------
get.coupon.amnt.hist <- function(cluster) {
  cluster = cluster - 1
  ggplot(DF_CLUSTER[DF_CLUSTER$cluster == cluster,], aes(x =  주문서쿠폰할인금액 )) +
    geom_histogram(fill = "cornflowerblue") +
    simple_background
}
coupon.amnt.hist = list()
coupon.amnt.hist.input = list()
for (i in 1:4) {
  coupon.amnt.hist[[i]] = get.coupon.amnt.hist(i)
  a = DF_CLUSTER[DF_CLUSTER$cluster == (i - 1), "주문서쿠폰할인금액"]
  coupon.amnt.hist.input[[i]] = c(a %>% min, a %>% max)
}
#----------------쿠폰사용액 평균-------------------------
get.coupon.amnt.mean <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$주문서쿠폰할인금액[DF_CLUSTER$cluster == cluster]) /
                10000, 1), " 만원")
}
coupon.amnt.mean = list()
for (i in 1:4) {
  coupon.amnt.mean[[i]] = get.coupon.amnt.mean(i)
}
#----------------쿠폰사용비율율--------------------------
get.coupon.use.ratio <- function(cluster) {
  cluster = cluster - 1
  paste(round(mean(DF_CLUSTER$쿠폰사용여부[DF_CLUSTER$cluster == cluster]) *
                100), "%")
}
coupon.use.ratio = list()
for (i in 1:4) {
  coupon.use.ratio[[i]] = get.coupon.use.ratio(i)
}

#--------------- 클러스터별 유저 수 --------------------------
get.user.count <- function(cluster_) {
  cluster_ = cluster_ - 1
  user.count <- DF_CLUSTER %>% filter(cluster == cluster_) %>% NROW
  round(user.count / NROW(DF_CLUSTER) * 100)
  paste0(user.count, " 명 (", round(user.count / NROW(DF_CLUSTER) * 100), "%)")
}
user.count = list()
for (i in 1:4) {
  user.count[[i]] = get.user.count(i)
}

#--------------- 3개월 매출 --------------------------
get.three.month.revenue <- function(cluster_) {
  cluster_ = cluster_ - 1
  three.month.revenue <- DF_CLUSTER %>%
    filter(cluster == cluster_) %>%
    summarise(three.month.revenue = sum(최근3개월주문액))
  three.month.revenue = three.month.revenue$three.month.revenue
  total = DF_CLUSTER %>% select("최근3개월주문액") %>% sum
  if (three.month.revenue > 100000000) {
    amnt = paste0(round(three.month.revenue / 100000000, 1), " 억")
  } else if (three.month.revenue > 10000) {
    amnt = paste0(round(three.month.revenue / 10000, 1), " 만")
  }
  paste0(amnt, " (", round(three.month.revenue / total * 100), "%)")
}
three.month.revenue = list()
for (i in 1:4) {
  three.month.revenue[[i]] = get.three.month.revenue(i)
}

#--------------- 1인당 3개월 구매액 --------------------------
get.single.3month.revenue <- function(cluster_) {
  cluster_ = cluster_ - 1
  three.month.revenue <- DF_CLUSTER %>%
    filter(cluster == cluster_) %>%
    summarise(three.month.revenue = mean(최근3개월주문액))
  three.month.revenue = three.month.revenue$three.month.revenue
  paste0(round(three.month.revenue / 10000, 1), " 만")
}
single.3month.revenue = list()
for (i in 1:4) {
  single.3month.revenue[[i]] = get.single.3month.revenue(i)
}

#--------------- Top 5 팔린 물건 --------------------------
get.most.sold.item.dt <- function(cluster_) {
  cluster_ = cluster_ - 1
  df <- DF_NEW_ITEM_NAME %>%
    filter(cluster == cluster_) %>%
    select(주문상품명) %>%
    table %>%
    sort(decreasing = TRUE) %>%
    data.frame(stringsAsFactors = FALSE)
  df = datatable(
    df,
    colnames = c("상품 이름", '판매수'),
    rownames = FALSE,
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 7, 10, 15, 20)
    )
  ) %>%
    formatStyle(
      'Freq',
      background = styleColorBar(df$Freq, "lightblue"),
      backgroundSize = '100%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
}
most.sold.item.dt = list()
for (i in 1:4) {
  most.sold.item.dt[[i]] = get.most.sold.item.dt(i)
}

#--------------- 클러스터별 주간 매출 & 시계열 그림--------------------------
weekly.revenue = list()
weekly.revenue.hist = list()
for (i in 1:4) {
  weekly.revenue[[i]] = DF_TOTAL %>%
    filter(cluster == (i - 1)) %>%
    group_by(week = floor_date(주문일시, "week")) %>%
    summarize(weekly.revenue = sum(주문액))
  weekly.revenue.hist[[i]] <-
    ggplot(
      weekly.revenue[[i]] %>% mutate(weekly.revenue2 = weekly.revenue / 10000),
      aes(x = week, y = weekly.revenue2)
    ) +
    geom_line(color = "cornflowerblue", size = 2) +
    simple_background +
    xlab("") +
    ylab("주별 매출(만 원)")
}

#--------------- 클러스터별 월별 매출 & 시계열 그림--------------------------
monthly.revenue = list()
monthly.revenue.hist = list()
for (i in 1:4) {
  monthly.revenue[[i]] = DF_TOTAL %>%
    filter(cluster == (i - 1)) %>%
    group_by(month = floor_date(주문일시, "month")) %>%
    summarize(monthly.revenue = sum(주문액))
  monthly.revenue.hist[[i]] <-
    ggplot(
      monthly.revenue[[i]] %>% mutate(monthly.revenue2 = monthly.revenue / 10000),
      aes(x = month, y = monthly.revenue)
    ) +
    geom_line(color = "cornflowerblue", size = 2) +
    simple_background +
    xlab("") +
    ylab("월별 매출(만 원)")
}
