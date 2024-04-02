#' Collect all functions in package
#'
#' @description collect all the functions defined in an R program or directory
#' @name collectFunNames
#'
#' @param x A character string specifying the path to an R package or directory
#' @param is.package Logical indicating whether the search path is an R package
#' or simply a directory containing `.r` or `.R` files.
#' Default: TRUE
#' @return A named list of top level functions in the program
#'
#' @export
collectFunNames <- function(x, is.package = TRUE){
    stopifnot(is.character(x), length(x) == 1, dir.exists(x))
    x <- normalizePath(x)
    if(isTRUE(is.package)){
        Rpath <- list.dirs(x)[grep("^R$", list.dirs(x, full.names = F))]
    } else {
        # search all directories paths that don't start with `.`
        Rpath <- grep("/\\.", list.dirs(x), value = T, invert = T)
    }
    if(length(Rpath) == 0 && isTRUE(is.package)){
        stop("`is.package = TRUE`, but no R directory found in path.")
    }
    Rscripts <- grep("\\.r$|\\.R$", list.files(Rpath, full.names = T), value = T)
    allFuns <- sapply(Rscripts, \(i){
        lines <- readLines(i, warn = F) |> stringr::str_trim() |>
            grep(pattern = "^#|^stop\\(", invert = T, value = T)
        funLines <- lapply(lines, \(x){
            grep("<- function", x, value = T) |> grep(pattern = "<-", value = T)
        }) |> purrr::compact()
        sapply(funLines, \(x) stringr::str_split(x, "<-", simplify = T)[,1]) |>
            stringr::str_trim() |> grep(pattern = "\\[|\\]", value = T, invert = T)
    }, simplify = FALSE, USE.NAMES = TRUE) |> purrr::compact()
    # handle the very special case of using this file as an example
    valid <- unique(unlist(allFuns, use.names = F)) |> 
        paste0(collapse = "|") |> 
        isValidRegex()
    if(!valid){ # find the offending function name
        invalid <- unique(unlist(allFuns, use.names = F)) |> 
            sapply(\(x) !isValidRegex(x)) |> 
            which()
        warning("Found invalid function call(s): ", toString(names(invalid)))
        allFuns <- lapply(allFuns, \(x){
            x[x %ni% names(invalid)]
        })
    } 
    return(allFuns)
}

#' Build a graph of an R package or directory
#'
#' @description Generates the Nodes and Edges of a set of functions in an R package or directory
#' @name buildPackageGraph
#'
#' @param x A character string specifying the path to an R package or directory
#' @param unique.edges Logical indicating whether there should be only a single 
#' edge between nodes.
#' DEFAULT: TRUE
#' @param is.package Logical indicating whether the search path is an R package
#' or simply a directory containing `.r` or `.R` files.
#' DEFAULT: TRUE
#' @param verbose Logical controlling verbosity of the function, mainly useful for
#' identifying which `.R` files contain no function calls. 
#' DEFAULT: FALSE
#' @return A named list of length 2 containing a character vector of nodes and a
#' data.frame of edges.
#'
#' @export
buildPackageGraph <- function(x, unique.edges = TRUE, is.package = TRUE, verbose = FALSE){
    stopifnot(is.character(x), length(x) == 1, dir.exists(x))
    stopifnot(is.logical(unique.edges), length(unique.edges) == 1)
    funs <- collectFunNames(x, is.package)
    allFuns <- unique(unlist(funs, use.names = F))
    res <- lapply(names(funs), \(z){
        thisFile <- readLines(z, warn = F) |> stringr::str_trim() |>
            grep(pattern = "^#|^stop\\(", invert = T, value = T)
        funLines <- grep(paste0(allFuns, collapse = "|"), thisFile)
        funValue <- thisFile[funLines]
        funAssLines <- grep(paste0(allFuns, ".*function", collapse = "|"), thisFile)
        funAssValue <- thisFile[funAssLines] |> # assignements, the function name comes first
            stringr::str_split("<-") |> purrr::map(.f = 1) |> unlist() |> stringr::str_trim()
        funCallLines <- funLines[funLines %ni% funAssLines]
        funCallValue <- funValue[funLines %ni% funAssLines] |>
            stringr::str_split("\\(| ", simplify = F) |>
            purrr::map(.f = \(x) grep(paste0(allFuns, collapse = "|"), x, value = T)) |>
            # TODO: this will lose a function call if more than one function is called
            #       on the same line. unsure how to solve at this stage
            purrr::map(.f = 1) |>
            unlist() |> # remove any leftover commas
            stringr::str_remove(pattern = ",|\\)")
        # check for some bad calls (not exhaustive)
        badCallLine <- grep(pattern = paste0("^", allFuns, "$", collapse = "|"),
                            x = funCallValue, invert = T)
        if(length(badCallLine) > 0){
            # if there are bad calls, need to
            funLines <- funLines[funLines %ni% funCallLines[badCallLine]]
            funValue <- funValue[funLines %ni% funCallLines[badCallLine]]
            funCallValue <- funCallValue[-badCallLine]
            funCallLines <- funCallLines[-badCallLine]
        }
        stopifnot(
            identical(funLines, c(funAssLines, funCallLines)[order(c(funAssLines, funCallLines))])
        )
        if(length(funCallLines) > 0){
            tmp <- sapply(1:length(funCallLines), \(i){
                if(length(which(funCallLines[i] > funAssLines)) == 0){
                    NULL
                } else {
                    funAssValue[max(which(funCallLines[i] > funAssLines))]
                }
            }) |> stats::setNames(object = funCallValue) 
            tmp[names(tmp) != "NULL"]
        } else {
            if(isTRUE(verbose)){
                warning("No function calls were found in file:", z, call. = F)
            }
            NULL
        }
    }) |> purrr::compact() |> unlist()
    res <- list(nodes = allFuns, edges = data.frame(from = unname(res), to = names(res)))
    if(unique.edges){
        res$edges <- dplyr::distinct(res$edges)
    }
    return(res)
}

#' Plot a graph or diagram of a package
#'
#' @description From a list of nodes and edges, plots a diagram or graph
#' @name plotPackageGraph
#'
#' @param x A character string specifying the path to an R package or directory.
#' @param graph A list generated by `buildPackageGraph`.
#' @param style A character string specifying the method to use for plotting
#' the graph. Options include c("grViz", "mermaid", "ggraph").
#' DEFAULT: "grViz"
#' @param gg.layout The type of layout to create. Either a valid string, a function, 
#' a matrix, or a data.frame (see `ggraph::ggraph` for Details).
#' DEFAULT: "tree"
#' @param ... Additional parameters passed to `buildPackageGraph` if `x` is supplied,
#' ignored otherwise
#' 
#' @details
#' Only one of `x` and `graph` needs to be specified, if `x` is given then `buildPackageGraph`
#' is called internally and `...` can be used to control its parameters. Otherwise if
#' `graph` is supplied then `x` is ignored. 
#' 
#' @return The side effect of plotting the graph.
#'
#' @export
plotPackageGraph <- function(x, graph, 
                             style = c("grViz", "mermaid", "ggraph"), 
                             gg.layout = "tree", 
                             ...){
    if(missing(x) & missing(graph)){
        stop("Either 'x' or 'graph' must be supplied")
    } else if(missing(x)){
        stopifnot(is.list(graph), all(c("nodes", "edges") %in% names(graph)))
    } else if(missing(graph)){
        graph <- buildPackageGraph(x, ...)
    } else {
        warning("'x' and 'graph' supplied, ignoring 'x' and using 'graph'")
    }
    
    stopifnot(is.list(graph), all(c("nodes", "edges") %in% names(graph)))
    style <- match.arg(style, c("grViz", "mermaid", "ggraph"))
    if(style == "grViz"){
        edges <- sapply(1:nrow(graph$edges), \(i){
            paste0(graph$edges$from[i], "->", graph$edges$to[i])
        }) |> paste0(collapse = ";") |> gsub(pattern = "\\.", replacement = "")
        # "." is not allowed in grViz node names
        nodes <- paste0(graph$nodes, collapse = ";") |> gsub(pattern = "\\.", replacement = "")
        fullGraph <- paste0(
            "digraph boxes_and_circles{", "\n",
            "graph [layout = dot, rankdir = LR]", "\n",
            "node [shape = box]", "\n",
            nodes, "\n",
            edges, "\n",
            "}"
        )
        DiagrammeR::grViz(
            fullGraph
        )
    } else if(style == "mermaid"){
        edges <- sapply(1:nrow(graph$edges), \(i){
            paste0(graph$edges$from[i], "-->", graph$edges$to[i])
        })
        fullGraph <- paste0(
            "graph LR", "\n",
            paste0(edges, collapse = "\n"), "\n"
        )
        DiagrammeR::mermaid(
            fullGraph
        )
    } else if(style == "ggraph"){
        graph$edges$relative_position <- 0.75
        gg <- igraph::graph_from_data_frame(d = graph$edges, directed = T, vertices = graph$nodes)
        lay <- ggraph::create_layout(gg, layout = gg.layout) 
        # lay2 <- updateLayout(lay)
        p <- ggraph::ggraph(lay) +
            ggraph::geom_edge_link() +
            ggraph::geom_edge_link(ggplot2::aes(xend=((xend-x)*relative_position)+x,
                                                yend=((yend-y)*relative_position)+y),
                                   arrow = ggplot2::arrow(
                                       angle = 10,
                                       length = ggplot2::unit(0.15, "inches"),
                                       ends = "last",
                                       type = "closed"
                                   )) +
            ggraph::geom_node_label(ggplot2::aes(label = name)) +
            ggraph::theme_graph() +
            ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.2, 0.2))) +
            ggplot2::coord_flip()
        print(p)
    }
}

#' rescale the x-axis of a ggraph tree diagram so that nodes don't overlap.
#' @noRd
#' @keywords internal
updateLayout <- function(layout){
    mx <- max(abs(layout$x))
    for(i in 0:max(layout$y)){
        rk <- rank(layout[layout$y == i,"x"], ties.method = "random")
        layout[layout$y == i,"x"] <- scales::rescale(rk, to = c(-mx, mx))
    }
    return(layout)
}

#' Not in
#'
#' @description Negation of the `%in%` operator
#'
#' @noRd
#' @keywords internal
"%ni%" <- Negate("%in%")

#' Check validity of regex expression
#'
#' @description Check validity of regex expression. Code used, unchanged, from
#' https://github.com/trinker/qdapRegex/releases/tag/v0.7.2
#'
#' @noRd
#' @keywords internal
isValidRegex <- function(pattern) {
    out <- suppressWarnings(
        try(gsub(pattern, "", "hello"), silent = TRUE)
    )
    return(ifelse(inherits(out, "try-error"), FALSE, TRUE))
}
