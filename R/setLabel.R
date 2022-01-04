#' Add label to a vector
#' @param x a vector
#' @param label string
#' @return a labelled vector
#' @export
setLabel=function(x,label=""){
     attr(x,"label")=label
     x
}
