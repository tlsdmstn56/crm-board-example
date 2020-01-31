source("dashboard-theme.R", encoding = "UTF-8")
source("cluster-tab.R", encoding = "UTF-8")
source("demand-prediction-tab.R", encoding = "UTF-8")
source("cluster-comparison.R", encoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "SOME COMPANY Customers"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem(
        "clusters Overviews",
        icon = icon("layer-group"),
        startExpanded = FALSE,
        menuSubItem("고객군 1", tabName = "cluster1"),
        menuSubItem("고객군 2", tabName = "cluster2"),
        menuSubItem("고객군 3", tabName = "cluster3"),
        menuSubItem("고객군 4", tabName = "cluster4")
      ),
      menuItem("Cluster Comparison", tabName = "cluster-cmp", icon = icon("columns")),
      menuItem("Demand Prediction", tabName = "demand-pred",  icon = icon("columns"))
    ),
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton",
      label = "Search..."
    )
  ),
  dashboardBody(
    dashboard_theme,
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h2("SOME COMPANY Client Home"),
              p("패커스 고객 정보를 한 눈에 관리하고 확인할 수 있는 사이트 입니다.")
              ),
      
      # Second tab content
      clusterTabUI("tab1",1),
      clusterTabUI("tab2",2),
      clusterTabUI("tab3",3),
      clusterTabUI("tab4",4),
      # Second tab content
      tabItem(tabName = "by-columns",
              h2("Widgets tab content")),
      demandPredictionUI("demand","demand-pred"),
      clusterComparisonUI("cc","cluster-cmp")
    )
  )
)
