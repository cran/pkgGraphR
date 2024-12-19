# pkgGraphR

[![CRAN
status](https://www.r-pkg.org/badges/version/pkgGraphR)](https://CRAN.R-project.org/package=pkgGraphR)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Static Badge](https://img.shields.io/badge/code_coverage-97.52%25-brightgreen)](https://covr.r-lib.org/)

## Use Case

When developing large packages or Shiny apps in R, it can be difficult to track
where a modification to one function might propagate down-stream. As a
simple example, suppose you've moved from the initial prototype phase into a 
more serious development phase for a package and realize that you would like to
change parameter names in a function from `some.param` to `someParam` in order
to match other function parameterization. This function might live in the 
`utils.R` file and be called by functions spread out across several other `.R` 
files. Having a map (graph) of what functions depend on this function can be 
very helpful in making sure that all usages of this function get adjusted correctly.

As this package is specifically intended for development phase, it doesn't 
require the package to be built and in fact works with any directory containing
R files, or even a single '.R' file. This has the advantage of being useful outside 
of R package development, specifically in cases such as shiny apps which are often 
developed outside of the package context. 

## Install

`install.packages("pkgGraphR")`
or
`devtools::install_gitlab("doliv071/pkggraphr")`

## Usage

There are 2 (or 3) steps to using `pkgGraphR`, first (optional) collect the function 
assignments with `collectFunNames`, next build the graph object (a list containing 
`nodes` and `edges`) with `buildPackageGraph`, finally visualize the results as 
desired with `plotPackageGraph`. The example below shows how to use each function
assuming you are in the package or app directory you want to visualize.

```r
library(pkgGraphR)

funclist <- collectFunNames(x = ".")
funcgraph <- buildPackageGraph(x = ".", 
                               unique.edges = TRUE, 
                               only.connected = FALSE)
# under default parameters, only the graph is required
plotPackageGraph(graph = funcgraph)
# alternatively, plot with grouping and/or coloring (requires fun.list)
plotPackageGraph(graph = funcgraph, 
                 fun.list = funclist, 
                 use.colors = T)
plotPackageGraph(graph = funcgraph, 
                 fun.list = funclist, 
                 use.subgraphs = T)
plotPackageGraph(graph = funcgraph, 
                 fun.list = funclist, 
                 use.subgraphs = T, 
                 use.colors = T)

```

![Default Parameters](img/default.png "Default Example"){width=24% height=250px} ![With Color Parameter](img/colored.png "Color by Source Example"){width=24% height=250px} ![With Subgraphs Parameter](img/subgraph.png "Subgraphs by Source Example"){width=24% height=250px} ![With Subgraphs and Color Parameter](img/color_subgraph.png "Subgraphs and Color by Source Example"){width=24% height=250px}

### Known "issues"

There are a few known issues which should be taken into consideration. 

- `grViz` doesn't allow `.` in node names so if you use `my.function` be aware 
that `grViz` will show these as `myfunction`. 
- The `use.colors` parameter does not come with a legend, which isn't ideal.
- Very large packages (e.g. `dplyr`) will be difficult to visualize. As a 
workaround, you can use `htmlwidgets` and `webshot` to generate a high resolution
pdf as below.

```r
p <- plotPackageGraph(graph = funcgraph)
htmlwidgets::saveWidget(p, "test.html")
webshot::webshot(url = "test.html", file = "test.pdf")

```


