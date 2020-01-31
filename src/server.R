source("cluster-tab.R", encoding = "UTF-8")
source("cluster-comparison.R", encoding = "UTF-8")

server <- function(input, output, session) {
  # -------------------------------------------------
  # for each cluster
  # -------------------------------------------------
  for (i in 1:4) {
    callModule(clusterTab, paste0("tab",i))
  }
  callModule(demandPrediction,"demand")
  callModule(clusterComparison,"cc")
}