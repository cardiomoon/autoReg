#' Makes table summarizing list of models
#' @param fitlist A list of objects of class "coxph"
#' @return No return value, called for side effects
#' @export
#' @examples
#' library(survival)
#'fit1=coxph(Surv(time,status) ~rx,data=anderson)
#'fit2=coxph(Surv(time,status) ~rx+logWBC,data=anderson)
#'fit3=coxph(Surv(time,status) ~rx*logWBC,data=anderson)
#'fitlist=list(fit1,fit2,fit3)
#'modelsSummary(fitlist)
modelsSummary=function(fitlist){
     dflist=map(fitlist,gaze)
     count=length(fitlist)
     lik=map_chr(fitlist,fit2lik)
     label=paste("Model",1:count)
     df=reduce(dflist,rbind)
     df=as.data.frame(df)
     df
     names(df)[1]=" "
     for(i in 2:4){
          df[[i]]=sprintf("%.03f",df[[i]])
     }
     df[[5]]=p2character2(df[[5]],add.p=FALSE)
     for(i in 6:8){
          df[[i]]=sprintf("%.02f",df[[i]])
     }
     df

     no=lapply(dflist,nrow)
     lengths1=map_int(df,maxnchar)
     lengths2=map_int(names(df),maxnchar)
     lengths=pmax(lengths1,lengths2)+2
     lineno=sum(lengths)

     collength=ncol(df)
     side=c(rep("right",1),rep("left",collength-1))

     list(names(df),lengths,side) %>% pmap_chr(str_pad) -> header
     drawline(lineno);cat("\n")
     cat(paste0(header,collapse=""),"\n")
     drawline(lineno);cat("\n")

     list(df,lengths,side) %>% pmap_dfc(str_pad) ->df1
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
     for(i in 1:count){
          cat("Model ",i,":",attr(dflist[[i]],"call"),"\n")
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
     names(df)[1]=" "
     for(i in 2:4){
          df[[i]]=sprintf("%.03f",df[[i]])
     }
     df[[5]]=p2character2(df[[5]],add.p=FALSE)
     for(i in 6:8){
          df[[i]]=sprintf("%.02f",df[[i]])
     }
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
