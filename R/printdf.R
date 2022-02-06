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
     printdf(x,...)
     if(!is.null(attr(x,"lik"))) cat(paste0(attr(x,"lik")," "))
     if(!is.null(attr(x,"dev"))) cat(paste0(attr(x,"dev")," "))
     if(!is.null(attr(x,"add"))) cat(paste0(attr(x,"add"),collapse=","),"\n")
}



#'Print function for data.frame
#'@param x A data.frame
#'@param showid logical if TRUE, show id
#'@param rows Numeric Rows to be hightlighted
#'@param cols Numeric Columns to be hightlighted
#'@param highlight style of highlight
#'@param highlight.header logical Whether or not highlight header
#'@importFrom crayon red bold
#'@export
#'@examples
#' x=mtcars[1:5,1:5]
#' printdf(x)
#' printdf(x,cols=2)
#' printdf(x,rows=c(1,3),cols=c(1,3))
#' library(crayon)
#' printdf(x,rows=c(1,3),highlight=inverse)
#'@return No return value, called for side effects
printdf=function(x,showid=FALSE,cols=NULL,rows=NULL,highlight=NULL,highlight.header=FALSE){
       # cols=NULL;rows=NULL;highlight=NULL;row=3;cols=2:3
     if(is.null(highlight)) highlight=red $ bold
     if("autoReg" %in% class(x)) {
          if(is.null(attr(x,"summary"))){
               if(showid==FALSE) x$id=NULL
               names(x)[1]=paste0("Dependent: ",attr(x,"yvars"))
               names(x)[2]=" "
               if(attr(x,"model")=="coxph") names(x)[3]="all"
          } else if(attr(x,"model") %in% c("coxph","glm")){
               names(x)[1]=" "
               for(i in 2:4){
                    x[[i]]=sprintf("%.03f",x[[i]])
               }
               x[[5]]=p2character2(x[[5]],add.p=FALSE)
               for(i in 6:8){
                    x[[i]]=sprintf("%.02f",x[[i]])
               }
          } else{
               names(x)[1]=" "
               for(i in 2:4){
                    x[[i]]=sprintf("%.03f",x[[i]])
               }
               x[[5]]=p2character2(x[[5]],add.p=FALSE)
               for(i in 6:7){
                    x[[i]]=sprintf("%.03f",x[[i]])
               }
          }
     }
     x[]=lapply(x,function(y){
          if(is.factor(y)) y=as.character(y)
          y
     })
     lengths1=map_int(x,maxnchar)
     lengths2=map_int(names(x),maxnchar)
     lengths=pmax(lengths1,lengths2)+2
     lineno=sum(lengths)
     no=ncol(x)
     if("imputedReg" %in% class(x)) {
          side=c(rep("right",1),rep("left",no-1))
     } else if(!is.null(attr(x,"summary"))){
          side=c(rep("right",1),rep("left",no-1))
     } else{
          side=c()
          for(i in 1:ncol(x)){
               if(is.numeric(x[[i]])){
                    side=c(side,"left")
               } else{
                    side=c(side,"right")
               }
          }
     }
     list(names(x),lengths,side) %>% pmap_chr(str_pad) -> header

     if(!is.null(cols) & highlight.header) {
          header[cols]=highlight(header[cols])
     }
     drawline(lineno);cat("\n")
     cat(paste0(header,collapse=""),"\n")
     drawline(lineno);cat("\n")

     list(x,lengths,side) %>% pmap_dfc(str_pad) ->x1
     if(is.null(cols)){
          if(!is.null(rows)) cols=1:ncol(x1)
     }
     if(is.null(rows)){
          if(!is.null(cols)) rows=1:nrow(x1)
     }
     if(!is.null(rows) & !is.null(cols)){
          for(i in rows){
               for(j in cols){
                   x1[i,j]=highlight(x1[i,j])
               }
          }
     }
     for(i in 1:nrow(x)){
          cat(paste0(x1[i,],collapse=""),"\n")
     }
     drawline(lineno);cat("\n")
}


