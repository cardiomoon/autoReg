#' Makes table summarizing list of models
#' @param fitlist A list of objects of class "coxph"
#' @param show.lik logical Whether or not show likelihood test results
#' @return No return value, called for side effects
#' @export
#' @examples
#' library(survival)
#'fit1=coxph(Surv(time,status) ~rx,data=anderson)
#'fit2=coxph(Surv(time,status) ~rx+logWBC,data=anderson)
#'fit3=coxph(Surv(time,status) ~rx*logWBC,data=anderson)
#'fitlist=list(fit1,fit2,fit3)
#'modelsSummary(fitlist)
modelsSummary=function(fitlist,show.lik=FALSE){
     dflist=map(fitlist,gaze)
     count=length(fitlist)
     lik=map_chr(fitlist,fit2lik)
     label=paste("Model",1:count)
     df=reduce(dflist,rbind)
     df=as.data.frame(df)
     df
      class(df)=c("autoReg","data.frame")
      attr(df,"summary")=TRUE

     no=unlist(lapply(dflist,nrow))
     no

     df1=as_printable(df)
     df1
     temp=paste0(deparse(fitlist[[1]]$call),collapse="")
     dataname=gsub(")","",unlist(strsplit(temp,"data = "))[2])
     temp=paste0("\nDependent: ", attr(dflist[[3]],"yvar"),", data=",dataname)
     cat(temp,"\n")
     lineno=sum(nchar(names(df1)))
     drawline(lineno);cat("\n")
     cat(paste0(names(df1),collapse=""),"\n")
     drawline(lineno);cat("\n")

     start=1
     i=1
     for(i in 1:count){
          end=start+no[[i]]-1
          cat(label[i],"\n")
          for(j in start:end){
               cat(paste0(df1[j,],collapse=""),"\n")
          }
          drawline(lineno);cat("\n")
          start=start+no[[i]]
     }
     if(show.lik){
          footer=paste0(paste("Model",1:3,":",map_chr(fitlist,fit2lik),collapse="\n"))
          cat(footer,"\n")
     }
}

#' Makes flextable summarizing list of models
#' @param fitlist A list of objects of class "coxph"
#' @param labels character labels of models
#' @param show.lik logical Whether or not show likelihood test results
#' @return A flextable
#' @importFrom officer fp_border
#' @importFrom flextable merge_at add_footer_row as_grouped_data add_header_row
#' @export
#' @examples
#'library(survival)
#'fit1=coxph(Surv(time,status) ~rx,data=anderson)
#'fit2=coxph(Surv(time,status) ~rx+logWBC,data=anderson)
#'fit3=coxph(Surv(time,status) ~rx*logWBC,data=anderson)
#'fitlist=list(fit1,fit2,fit3)
#'modelsSummaryTable(fitlist)
modelsSummaryTable=function(fitlist,labels=NULL,show.lik=FALSE){
     dflist=map(fitlist,gaze)
     count=length(fitlist)
     lik=map_chr(fitlist,fit2lik)
     if(is.null(labels)) labels=paste("Model",1:count)
     for(i in 1:count){
          #dflist[[i]]$Model=paste("Model",i,": call=",attr(dflist[[i]],"call"))
          dflist[[i]]$Model=labels[i]
     }
     dflist
     df=reduce(dflist,rbind)
     df=as.data.frame(df)
     df
     no=unlist(lapply(dflist,nrow))
     no
     no1=cumsum(no)+(1:length(no))
     no1
     no1=no1[-length(no1)]
     no
     no2=cumsum(no)
     no2
     ft<-as_grouped_data(df,groups="Model") %>%
          flextable()
     for(i in seq_along(no2)){
          ft= merge_at(ft,i=no2[i],j=1:ncol(df))
     }

     ft=ft %>% hline(i=no1,border=fp_border(color="black",width=1))%>%
          align(align="center",part="header") %>%
          align(j=3:ncol(df),align="right")
     if(show.lik) {
          footer=paste0(paste("Model",1:3,":",map_chr(fitlist,fit2lik),collapse="\n"))
          ft<-ft %>% add_footer_row(values=footer,colwidths=ncol(df))
     }
     temp=paste0(deparse(fitlist[[1]]$call),collapse="")
     dataname=gsub(")","",unlist(strsplit(temp,"data = "))[2])
     header=paste0("Dependent: ", attr(dflist[[3]],"yvar"),", data=",dataname)
     ft<-ft %>% add_header_row(values=header,colwidths=ncol(df))
     ft<-ft %>% align(i=1,align="left",part="header") %>%
               hline_top(border=fp_border(color="black",width=0),part="header")
     ft
}
