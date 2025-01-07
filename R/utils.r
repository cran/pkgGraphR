#' find all functional assignments
#'
#' @name findFunNames
#'
#' @param x the parsed data from a source file. The output from
#' \code{\link[utils]{getParseData}}
#'
#' @noRd
#' @keywords internal
findFunNames <- function(x){
    len <- nrow(x) - 2
    res <- lapply(seq(len), \(i){
        if(x$token[i] == "SYMBOL"){
            if(x$token[i+1] == "LEFT_ASSIGN" || x$token[i+1] == "EQ_ASSIGN"){
                if(x$token[i+2] == "FUNCTION"){
                    c(line = x$line1[i], name = x$text[i])
                }
            }
        }
    }) |> purrr::compact() |> purrr::list_transpose() |> as.data.frame()
    res$line <- as.numeric(res$line)
    return(res)
}

#' find all functional calls and their calling function
#'
#' @name findFunCalls
#'
#' @param x A file path from which function calls should be scraped
#' @param fun.list A list of all function assignments generated by `collectFunNames`
#'
#' @noRd
#' @keywords internal
findFunCalls <- function(x, fun.list){
  xname <- x
  pR <- parse(x, keep.source = TRUE)
  pRD <- utils::getParseData(pR) |> dplyr::filter(token == "SYMBOL_FUNCTION_CALL")
  all.funs <- sapply(fun.list, \(x){ x$name }) |> unlist(use.names = FALSE) |> unique()
  pRDFunCalls <- pRD[which(pRD$text %in% all.funs),]
  res_part1 <- fun.list[[xname]][findInterval(pRDFunCalls$line1, fun.list[[xname]]$line),"name"]
  # name is parent function, value is called function
  res <- stats::setNames(pRDFunCalls$text, res_part1)
  return(res)
}

#' color palette when more than 12 are needed
#'
#' @noRd
#' @keywords internal
colors51 <- c("#E64B35","#4DBBD5","#F39B7F","#C8A1A1","#8491B4","#91D1C2","#A4E804",
              "#D790FF","#1CE6FF","#FF34FF","#3B5DFF","#8FB0FF","#D16100","#FF2F80",
              "#FFDBE5","#B79762","#4FC601","#FF4A46","#FFB500","#00C2A0","#FFAA92",
              "#FF90C9","#B903AA","#A1C299","#0AA6D8","#BA0900","#C2FFED","#A079BF",
              "#C2FF99","#0CBD66","#EEC3FF","#B77B68","#FAD09F","#FF8A9A","#D157A0",
              "#BEC459","#A3C8C9","#FF913F","#00FECF","#B05B6F","#8CD0FF","#04F757",
              "#7900D7","#FFF69F","#BC23FF","#99ADC0","#FDE8DC","#CB7E98","#B09C85",
              "#0086ED","#DC0000")
