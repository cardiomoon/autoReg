#' Convert data.frame to flextable
#'
#' @param df A data.frame
#' @param vanilla A Logical
#' @param fontname Font name
#' @param fontsize font size
#' @param add.rownames logical. Whether or not include rownames
#' @param even_header background color of even_header
#' @param odd_header background color of even_header
#' @param even_body background color of even_body
#' @param odd_body background color of even_body
#' @param vlines Logical. Whether or not draw vertical lines
#' @param colorheader Logical. Whether or not use color in header
#' @param digits integer indicating the number of decimal places
#' @param digitp integer indicating the number of decimal places of p values
#' @param align_header alignment of header. Expected value is one of 'left', 'right', 'center', 'justify'.
#' @param align_body alignment of body. Expected value is one of 'left', 'right', 'center', 'justify'.
#' @param align_rownames alignment of rownames. Expected value is one of 'left', 'right', 'center', 'justify'.
#' @param NA2space A logical. If true, convert NA value to space
#' @param pcol An integer indicating p value. If specified, convert value less than 0.01 to "< 0.001" in given column.
#' @param ... further arguments to be passed to flextable
#' @importFrom flextable flextable regulartable set_formatter_type set_header_df theme_zebra vline vline_left align autofit padding hline hline_top hline_bottom border_remove font fontsize color
#' @importFrom officer fp_border
#' @importFrom dplyr "%>%"
df2flextable=function(df,vanilla=FALSE,fontname=NULL,fontsize=12,
                      add.rownames=FALSE,
                      even_header="transparent",odd_header="#5B7778",
                      even_body="#EFEFEF",odd_body="transparent",
                      vlines=TRUE,colorheader=FALSE,digits=2,digitp=3,
                      align_header="center",align_body="right",
                      align_rownames="left",
                      NA2space=TRUE,pcol=NULL,...){

     # vanilla=FALSE;fontname=NULL;fontsize=12
     # add.rownames=FALSE
     # even_header="transparent";odd_header="#5B7778"
     # even_body="#EFEFEF";odd_body="transparent"
     # vlines=TRUE;colorheader=FALSE;digits=2
     # align_header="center";align_body="right"
     # NA2space=FALSE;pcol=NULL

     if(!is.null(pcol)){
          for(i in 1:length(pcol)){
               df[[pcol[i]]]=p2character2(df[[pcol[i]]],digitp,add.p=FALSE)
               # df[[pcol]]=sprintf(paste0("%.",digitp,"f"),df[[pcol]])
               #
               # temp=rep(0,digitp)
               # temp=stringr::str_c(temp,collapse="")
               # lookfor=paste0("0.",temp)
               # rep=paste0("< 0.",stringr::str_c(rep(0,digitp-1),collapse=""),"1")
               # rep
               # df[[pcol]][df[[pcol]]==lookfor]=rep
          }
     }

     if(add.rownames){
          df<-cbind(rowname=rownames(df),df)
          if(length(digits)!=1) digits=c(0,digits)
     }
     df<-roundDf(df,digits)
     if(NA2space) {
          df[is.na(df)]=""
          df[df=="NA"]=""
     }
     if(!colorheader){
          headercolor=ifelse(vanilla,"black","white")
     } else{
          headercolor=ifelse(vanilla,"#007FA6","white")
     }
     df
     big_border=fp_border(color=headercolor,width=2)
     header_border=fp_border(color="black",width=1)
     std_color="#EDBD3E"
     if(vanilla) std_color="black"
     std_border=fp_border(color=std_color,width=1)

     # fmt_double=paste0("%0.0",sprintf("%df",digits))

     # ft <- regulartable(df,...) %>% set_formatter_type(fmt_double=fmt_double)
     # ft <- flextable(df) %>% set_formatter_type(fmt_double=fmt_double)
     df
     ft<-tryCatch(regulartable(df),error=function(e) "error")
     if("character" %in% class(ft)) {
          ft=flextable(df)
     }
     # } else{
     #     result=tryCatch(print(ft),error=function(e) "error")
     #     if("character" %in% class(result)) ft=flextable(df)
     # }

     odd_header=ifelse(vanilla,"transparent","#5B7778")
     if(!vanilla)
          ft<- ft %>% theme_zebra(even_body=even_body,odd_body=odd_body,
                                  odd_header = odd_header)
     ft <- ft %>% border_remove()
     if(!is.null(fontname)) ft<-ft %>% font(fontname=fontname,part="all")
     ft<-ft %>%
          fontsize(size=fontsize+1,part="header") %>%
          fontsize(size=fontsize,part="body") %>%
          color(color=headercolor,part="header") %>%
          color(color="black",part="body")

     if(vanilla){
          ft <- ft %>% hline_top(part="all",border=big_border) %>%
               hline_bottom(part="all",border=big_border)

     } else{
          ft <- ft %>% hline_top(part="all",border=header_border) %>%
               hline_bottom(part="all",border=std_border)
     }
     if(!vanilla){
          ft <- ft %>% hline(part="body",border=std_border)
     }
     if((!vanilla)&(vlines)){
          ft <-ft %>% vline(part="body",border=std_border) %>%
               vline_left(part="body",border=std_border) %>%
               vline(part="header",border=header_border) %>%
               vline_left(part="header",border=header_border)
     }
     ft <- ft %>% align(align=align_body,part="body") %>%
          align(align=align_header,part="header") %>%
          padding(padding.left=5,padding.right=5,
                  padding.top=2,padding.bottom=2,part="all")
     if(add.rownames) {
          ft<-ft %>%
               color(i=1,j=1,color=ifelse(vanilla,"white","#5B7778"),part="header") %>%
               align(j=1,align=align_rownames)
     }
     ft
}

#'Convert numeric columns of data.frame to character
#'@param df a data.frame
#'@param digits integer indicating the number of decimal places
roundDf=function(df,digits=2){
     if(length(digits)==1){
          digits<-rep(digits,ncol(df))
     }
     else if(length(digits)!=ncol(df)) {
          digits<-c(digits,rep(0,ncol(df)-length(digits)))
     }
     df[]<-lapply(1:ncol(df),function(i){
          if(is.integer(df[[i]])) {
               df[[i]]<-df[[i]]
          } else if(is.numeric(df[[i]])) {
               fmt=paste0("%0.",sprintf("%d",digits[i]),"f")
               df[[i]]=sprintf(fmt,df[[i]])
          } else{
               df[[i]]<-df[[i]]
          }

     })
     df
}

