## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----Usage, eval=FALSE--------------------------------------------------------
# 
# library(pkgGraphR)
# 
# funclist <- collectFunNames(x = ".")
# funcgraph <- buildPackageGraph(x = ".",
#                                unique.edges = TRUE,
#                                only.connected = FALSE)
# # under default parameters, only the graph is required
# plotPackageGraph(graph = funcgraph)
# # alternatively, plot with grouping and/or coloring (requires fun.list)
# plotPackageGraph(graph = funcgraph,
#                  fun.list = funclist,
#                  use.subgraphs = T,
#                  use.colors = T)
# 

## ----save PDF, eval=FALSE-----------------------------------------------------
# 
# p <- plotPackageGraph(graph = funcgraph)
# htmlwidgets::saveWidget(p, "test.html")
# webshot::webshot(url = "test.html", file = "test.pdf")
# 

