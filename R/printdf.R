#' S3 method print for an object of class autoReg
#' @param x An object of class autoReg
#' @param ... Further arguments
#' @examples
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' autoReg(fit)
#' @return No return value, called for side effects
#' @export
print.autoReg=function(x,...){
     if(is.null(attr(x,"summary"))) x=myformat(x)
     printdf(x,...)
     if(!is.null(attr(x,"lik"))) cat(paste0(attr(x,"lik")," "))
     if(!is.null(attr(x,"dev"))) cat(paste0(attr(x,"dev")," "))
     if(!is.null(attr(x,"add"))) cat(paste0(attr(x,"add"),collapse=","),"\n")
}



#'Print function for data.frame
#'@param x A data.frame
#'@importFrom crayon col_nchar
#'@export
#'@examples
#' x=mtcars[1:5,1:5]
#' printdf(x)
#'@return No return value, called for side effects
printdf=function(x){
        # showid=FALSE

     x=as_printable(x)
     lineno=sum(col_nchar(names(x)))
     drawline(lineno);cat("\n")
     cat(paste0(colnames(x),collapse=""),"\n")
     drawline(lineno);cat("\n")
     for(i in 1:nrow(x)){
          cat(paste0(x[i,],collapse=""),"\n")
     }
     drawline(lineno);cat("\n")
}


