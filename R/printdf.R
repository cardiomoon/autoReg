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
     printdf(x)
     if(!is.null(attr(x,"lik"))) cat(paste0(attr(x,"lik")," "))
     if(!is.null(attr(x,"add"))) cat(paste0(attr(x,"add"),collapse=","),"\n")
}



#'Print function for data.frame
#'@param x A data.frame
#'@param showid logical if TRUE, show id
#'@return No return value, called for side effects
printdf=function(x,showid=FALSE){

     if(("autoReg" %in% class(x))&(showid==FALSE)) x$id=NULL
     if("autoReg" %in% class(x)) {
          names(x)[1]=paste0("Dependent: ",attr(x,"yvars"))
          names(x)[2]=" "
          if(attr(x,"model")=="coxph") names(x)[3]="all"
     }
     lengths1=map_int(x,maxnchar)
     lengths2=map_int(names(x),maxnchar)
     lengths=pmax(lengths1,lengths2)+2
     lineno=sum(lengths)
     no=ncol(x)
     side=rep("both",no)
     list(names(x),lengths,side) %>% pmap_chr(str_pad) -> header
     drawline(lineno);cat("\n")
     cat(paste0(header,collapse=""),"\n")
     drawline(lineno);cat("\n")
     if("imputedReg" %in% class(x)) {
          side=c(rep("right",1),rep("left",no-1))
     } else {
          side=c(rep("right",2),rep("left",no-2))
     }
     list(x,lengths,side) %>% pmap_dfc(str_pad) ->x1
     for(i in 1:nrow(x)){
          cat(paste0(x1[i,],collapse=""),"\n")
     }
     drawline(lineno);cat("\n")
}
