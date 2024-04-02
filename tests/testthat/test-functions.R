test_that("collectFunNames works", {
    system.file("extdata", package = "pkgGraphR") |> 
        collectFunNames() |> 
        expect_type("list") |> 
        expect_length(1) |> 
        expect_named()
    file.path(system.file("extdata", package = "pkgGraphR"), "functions.r") |> 
        collectFunNames() |> 
        expect_type("list") |> 
        expect_length(1) |> 
        expect_named()
})

test_that("buildPackageGraph works", {
    system.file("extdata", package = "pkgGraphR") |> 
        buildPackageGraph() |> 
        expect_type("list") |> 
        expect_length(2) |> 
        expect_named()
    system.file("extdata", package = "pkgGraphR") |> 
        buildPackageGraph(unique.edges = FALSE, only.connected = TRUE) |> 
        expect_type("list") |> 
        expect_length(2) |> 
        expect_named()
})

test_that("plotPackageGraph works", {
    system.file("extdata", package = "pkgGraphR") |> 
        plotPackageGraph() |> 
        expect_error()
    tF <- system.file("extdata", package = "pkgGraphR") |> 
        collectFunNames()
    tG <- system.file("extdata", package = "pkgGraphR") |> 
        buildPackageGraph() 
    
    plotPackageGraph(graph = tG) |> 
        inherits("grViz") |> 
        expect_equal(TRUE)
    plotPackageGraph(graph = tG, use.subgraphs = TRUE) |> 
        expect_error()
    plotPackageGraph(graph = tG, fun.list = tF, use.subgraphs = TRUE) |> 
        inherits("grViz") |> 
        expect_equal(TRUE)
    plotPackageGraph(graph = tG, fun.list = tF, use.colors = TRUE) |> 
        inherits("grViz") |> 
        expect_equal(TRUE)
    plotPackageGraph(graph = tG, fun.list = tF, use.colors = TRUE, use.subgraphs = TRUE) |> 
        inherits("grViz") |> 
        expect_equal(TRUE)

})
