#' Convert data.frame into flextable
#' @param x A data.frame
#' @param vanilla logical
#' @param  fontsize Numeric
#' @param digits integer indicating the position of decimal place
#' @param showid logical if TRUE, show id
#' @param ... Further arguments to be passed to df2flextable()
#' @importFrom flextable align autofit hline hline_top footnote as_paragraph
#' @importFrom officer fp_border
#' @importFrom purrr map_chr
#' @examples
#' data(acs,package="moonBook")
#' library(dplyr)
#' gaze(acs) %>% myft()
#' gaze(sex~.,acs) %>% myft()
#' \donttest{
#' gaze(sex+Dx~.,data=acs,show.p=TRUE,show.total=TRUE,show.n=TRUE,shiw.missing=TRUE) %>% myft()
#' gaze(Dx+sex~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' gaze(Dx+sex+HBP~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' }
#' @return An object of class \code{\link[flextable]{flextable}}
#' @export
myft=function(x,vanilla=TRUE,fontsize=10,digits,showid=FALSE,...){

     if("imputedReg" %in% class(x)){
          if(missing(digits)) digits=c(1,4,4,4,2,4,4,4,4,4,4,1)
     } else if("autoReg" %in% class(x)) {
          if(showid==FALSE) x$id=NULL
          if(names(x)[1]=="name"){
               names(x)[1]=paste0("Dependent: \n",attr(x,"yvars"))
          }
          names(x)[2]=" "
          if(attr(x,"model")=="coxph") names(x)[3]="all"
          if(missing(digits)) digits=2


     } else if(("gaze" %in% class(x))&(showid==FALSE)) {
          x$id=NULL
          names(x)[2]="levels"
          if(missing(digits)) digits=2
     }
     yvars=attr(x,"yvars")
     yvars
     if(length(yvars)>0){
          if(!is.null(attr(x,"missing"))) {
               yname=str_remove(attr(x,"yvars"),"Missing")
               names(x)[1]=paste0("Dependent:",yname)
          }

     }
     vanilla=TRUE
     ft<-x %>% df2flextable(vanilla=vanilla,fontsize=fontsize,digits=digits,...)


     if(length(yvars)>1){
          small_border = fp_border(color="gray", width = 1)
          df=attr(x,"groups")
          if(length(yvars)==2) {
               collapse=" "
          } else {
               collapse="\n"
          }
          groupvar=paste0(c(yvars[-length(yvars)],"(N)"),collapse=collapse)
          header=c(groupvar,map_chr(1:nrow(df),function(i){paste0(df[i,],collapse=collapse)}))
          length=nrow(df)
          no=(ncol(x)-2)%/%length
          widths=c(2,rep(no,length))

          ft<-ft %>%
               flextable::add_header_row(values=header,colwidths=widths) %>%
               hline(i=1,border=fp_border(color="gray", width = 0),part="header") %>%
               hline(i=1,border=fp_border(color="gray", width = 1),part="header") %>%
               hline_top(border=fp_border(color="black", width = 2),part="header")


     }
     if(("autoReg" %in% class(x))&(!is.null(attr(x,"add")))) {
          ft=footnote(ft,i=1,j=1,value=as_paragraph(paste0(attr(x,"add"),collapse=",")),ref_symbols="",part="body")
     }
     ft %>%
          flextable::align(align="center",part="header") %>%
          flextable::align(j=1:2,align="left",part="body") %>%
          flextable::autofit()
}
