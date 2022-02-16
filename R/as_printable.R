#' Convert data.frame to printable format
#'
#' Convert data.frame to printable format
#' @param x A data.frame
#' @param showid logical if TRUE, show id
#' @param digits Integer indicating the number of decimal places
#' @return A data.frame
#' @export
#' @examples
#' fit=lm(mpg~wt*hp,data=mtcars)
#' gaze(fit) %>% myformat()
myformat=function(x,showid=FALSE,digits=3){
     x[]=lapply(x,function(y){
          if(is.factor(y)) y=as.character(y)
          y
     })
     if("autoReg" %in% class(x)) {
          if(is.null(attr(x,"summary"))){
               if(showid==FALSE) x$id=NULL
          }}
     if("autoReg" %in% class(x)) {
          if(is.null(attr(x,"summary"))){
               names(x)[1]=paste0("Dependent: ",attr(x,"yvars"))
               names(x)[2]=" "
               if(attr(x,"model")=="coxph") names(x)[3]="all"
          } else{
               names(x)[1]=" "
               x[[5]]=p2character2(x[[5]],add.p=FALSE)

          }
     }
     if(!is.null(digits)){
          x[]=lapply(x,function(y){
               fmt=paste0("%.0",digits,"f")
               if(is.numeric(y)) {
                    if(!is.integer(y)) {y=sprintf(fmt,y)}
               }
               y
          })
     }
     x
}

#' Convert data.frame to printable form
#'
#' Calculate character length and apply all data
#' @param data A data.frame
#' @param align.first character Alignment of first variable
#' @param align.chr character Alignment of character variable
#' @param align.num character Alignment of numeric variable
#' @return A data.frame
#' @importFrom crayon col_align
#' @importFrom purrr map_int map_dfc
#' @export
#' @examples
#' as_printable(mtcars)
#' as_printable(iris)
as_printable=function(data,align.first="left",align.chr="right",align.num="right"){
     # showid=FALSE; align.first="left";align.chr="left";align.num="right";digits=3
     x=data
     x[is.na(x)]=""
     x[x=="NA"]=""
     side=align.first
     if(ncol(x)>1){
     for(i in 2:ncol(x)){
          if(is.numeric(x[[i]])){
               side=c(side,align.num)
          } else{
               side=c(side,align.chr)
          }
     }
     }
     lengths1=map_int(x,maxnchar)
     lengths2=map_int(names(x),maxnchar)
     lengths=pmax(lengths1,lengths2)+2
     lineno=sum(lengths)
     no=ncol(x)
     list(names(x),lengths,side) %>% pmap_chr(col_align) -> names(x)
     list(x,lengths,side) %>% pmap_dfc(col_align) ->x
     x
}


