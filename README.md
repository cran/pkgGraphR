# pkgGraphR

## Install

`devtools::install_gitlab("doliv071/pkggraphr")`

## Usage

```
library("pkgGraphR")
test <- buildPackageGraph("~/some/rpackage/")
plotPackageGraph(test, "grViz")
plotPackageGraph(test, "mermaid")
plotPackageGraph(test, "ggraph")
```

## Known Issues

1. When function assignment occurs within another function, this relationship is missed.  
2. Functions that are exclusively used in conjunction with logic operators are missed (e.g. `is.empty` function only called with `!` a la `!is.empty(var)`).  

### Notes

This package is pre-development stage. Please feel free to report issues when it fails (it will)

