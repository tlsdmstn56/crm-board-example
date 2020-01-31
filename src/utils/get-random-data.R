# randomize data
library(readr)
library(stringi)
library(dplyr)
library(wakefield)
library(igraph)
library(Matrix)

NUM_ITEMS = 160
ITEM_NAMES= paste0("item ", 1:NUM_ITEMS)

# 주문서 쿠폰 할인 금액
rand.int <- function(round, min, max,n)
{
  return(round(runif(n, min = min, max = max),round))
}

rand.zero.one <- function(n, prob)
{
  return(sample(x=0:1, size=n, prob = c(1-prob, prob),replace = TRUE))
}

rand.discrete.exp <- function(n)
{
  return(ceiling(rexp(n, rate=.01)))
}

# DF_CLUSTER
generate.df.cluster <- function(col.size = 8271)
{
  return(wakefield::r_data_frame(
    n = col.size,
    `아이디` = string(),
    `cluster`= level(x=0:3),
    `누적주문건수` = level(x=0:100),
    `총사용적립금` = level(x=1:70000),
    `총구매금액` = level(x=1:100000000),
    `추천인아이디` = string(),
    `평생회원` = level(0:1,prob = c(0.8,0.2)),
    `회원구분_x` = level(x=c("개인","사업자"), prob = c(0.8,0.2)),
    `회원등급` = level(x=c("개인","사업자","기타","전화주문"), prob = c(0.6,0.2,.1,.1)),
    `회원추가항목_가입경로` = level(x=c("없음","지인","인터넷","잡지","인터넷검색","전단지"), prob=c(0.5,.05,.05,0.2,.1,.1)),
    `PC_` = runif,
    `주문서쿠폰할인금액` = rand.int(-3, 0, 11000),
    `이용기간` = rand.int(0,0,500),
    `최근접속` = rand.int(0,0,100),
    `적립금_사용_거래_비율` = runif,
    `이용기간대비누적주문건수` = runif,
    `쿠폰사용여부`=rand.zero.one(0.05),
    `1월3월구매액`= rexp(rate = 0.00001),
    `재개편등급`= level(x=c("Bronze","Silver","Gold","VIP","VVIP"), prob = c(0.6, 0.3, 0.05, 0.0295,0.005)),
    `최근3개월주문액`= rexp(rate = 0.00001)
  ) %>% r_na(cols=c(6), prob = 0.99)
  %>% r_na(cols=c(16), prob = 0.23)
  %>% mutate(고객군=cluster)
  )  
}


generate.df.total <- function(col.size = 20000)
{
  return(
    
    wakefield::r_data_frame(
    n = col.size,
    `cluster`= level(x=0:3),
    `주문자ID` = string(),
    `사용한 적립금액`= runif,
    `총_결제금액`= runif,
    `사용한_적립금액`= runif,
    `적립금사용` = rand.zero.one(0.001),
    `주문액` = rexp(rate = 0.00001),
    `주문일시` = date_stamp(start = as.Date("2001-01-01"), k=round(col.size/10000),by="days")
    ) 
  )
}
random.item.names <- function(n, n.items)
{
  ratio = rexp(n.items, 0.01)
  return (sample(x=ITEM_NAMES, size=n, replace=TRUE, prob=ratio/sum(ratio)))
}
generate.df.new.item.name <- function(col.size=20000)
{
  return(
    wakefield::r_data_frame(
      n = col.size,
      `cluster`= level(x=0:3),
      `주문상품명` = random.item.names(n.items = NUM_ITEMS)
    )
  )
}

generate.df.prod.table <- function() {
  return(
    wakefield::r_data_frame(
      n = col.size,
      `prod_num`= 1:NUM_ITEMS,
      `prod_name` = ITEM_NAMES
    )
  )
}

generate.df.pred <- function(col.size=10000) {
  ret = wakefield::r_data_frame(
    n = col.size,
    `prod_num` = level(x=1:160),
    `수량` = rand.discrete.exp,
    `date` = date_stamp(x = seq(as.Date("2001-01-01"), length = 60, by = "1 months"))
  )
  ret = ret %>% add_row(prod_num=1:NUM_ITEMS, 
                        수량=rand.discrete.exp(NUM_ITEMS), 
                        date=date_stamp(NUM_ITEMS, x = seq(as.Date("2006-01-01"), length = 1, by = "1 months")))
  ret["year"] = year(ret$date)
  ret["month"] = month(ret$date)
  return (ret)
}

generate.graph.img<-function(filename, size=30)
{
  tmp = rand.discrete.exp(size**2) %>% scales::rescale(to=c(1, 30))
  tmp.mean = mean(tmp)
  mat = matrix(ifelse(tmp<tmp.mean,yes = tmp, 0), ncol=size) 
  selected.items = sample(x=ITEM_NAMES,size=size)
  colnames(mat) = selected.items
  rownames(mat) = selected.items
  diag(mat) <- 0
  net = mat %>% 
    forceSymmetric %>% 
    graph_from_adjacency_matrix(mode="undirected", weighted=TRUE, diag = FALSE)
  E(net)$width = E(net)$weight
  png(filename=filename, height = 611, width = 860)
  par(mar=c(0,0,0,0))
  plot(net, edge.arrow.size=.2, edge.curved=0,
       vertex.color="orange", vertex.frame.color="#555555",
       vertex.label=names(V(net)), vertex.label.color="black",
       vertex.lebel.cex=2, asp=0) 
  dev.off()
}
