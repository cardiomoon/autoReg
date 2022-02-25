#' Extract survival data from an object of class "survfit"
#' @param fit An object of class "survfit"
#' @param labels Character
#' @importFrom purrr map_dfc
#' @return A data.frame
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#'fit=survfit(coxph(Surv(time,status)~sex+age+strata(rx),data=colon))
#'survfit2df(fit)
#'fit=coxph(Surv(time,status)~sex+age+strata(rx),data=colon)
#'fit=survfit(as.formula(deparse(fit$terms)),data=fit2model(fit))
#'survfit2df(fit)
#'fit=survfit(Surv(time,status)~rx+sex+age,data=colon)
#'survfit2df(fit)
#'fit=survfit(Surv(time,status)~1,data=colon)
#'survfit2df(fit)
survfit2df=function(fit,labels=NULL){
     if(!is.null(fit$strata)){
          if(!is.null(labels)) {
               names(fit$strata)=labels
          }
         cols=c("time","n.risk","n.event","n.censor","surv","std.err","upper","lower")
         suppressMessages(res<-map_dfc(cols,~fit[[.]]))
         names(res)=cols

          strata=c()
          for(i in seq_along(fit$strata)){
               x=fit$strata[i]
               strata=c(strata,rep(names(x),x))
          }
          res$strata=strata
          suppressMessages(temp<-map_dfc(strata,~strsplit(.,", ")))
          stratalist=list()
          for(i in 1:nrow(temp)){
               x=strsplit(as.character(temp[i,1]),"=")[[1]][1]
               stratalist[[x]]=stringr::str_replace(temp[i,],".*=","")
          }
          res=cbind(res,as.data.frame(stratalist))

          start=1
          for(i in seq_along(fit$strata)){
               if(i==1){
                   temp=data.frame(0,fit$n[i],0,0,1,0,1,1)
                   temp=cbind(temp,res[start,9:ncol(res)])
                   names(temp)=names(res)
               } else{
                    temp1=data.frame(0,fit$n[i],0,0,1,0,1,1)
                    temp1=cbind(temp1,res[start,9:ncol(res)])
                    names(temp1)=names(res)
                    temp=rbind(temp,temp1)
               }
               temp=rbind(temp,res[start:(start+fit$strata[i]-1),])
               start=start+fit$strata[i]
          }
          res=temp
     } else{
          cols=c("time","n.risk","n.event","n.censor")
          suppressMessages(res<-map_dfc(cols,~fit[[.]]))
          df=data.frame(time=0,n.risk=fit$n,n.event=0,n.censor=0)
          names(res)=cols
          res=rbind(df,res)
          cols=c("surv","std.err","upper","lower")
          strata=attr(fit$surv,"dimnames")[[2]]
          no=length(strata)
          if(no>0){
          suppressMessages(df<-map_dfc(cols,function(x){
               temp=c()
               for(j in 1:no){
                   temp=c(temp,ifelse(x=="std.err",0,1))
                   temp=c(temp,fit[[x]][,j])
               }
               temp
          }))
               temp=rep(strata,each=nrow(res))
          } else{
               strata="all"
               df1=data.frame(surv=1,std.err=0,upper=1,lower=1)
               suppressMessages(df<-map_dfc(cols,~fit[[.]]))
               names(df)=cols
               df=rbind(df1,df)
               temp=rep(strata,nrow(df))
          }
          names(df)=cols
          df$strata=temp
          df1=res
          if(no>1){
          for(i in 2:no){
               df1=rbind(df1,res)
          }
          }
          res=cbind(df1,df)
          if(!is.null(labels)) {
               res$strata=rep(labels,each=nrow(res)/no)
               strata=res$strata
               suppressMessages(temp<-map_dfc(strata,~strsplit(.,", ")))
               stratalist=list()
               for(i in 1:nrow(temp)){
                    x=strsplit(as.character(temp[i,1]),"=")[[1]][1]
                    stratalist[[x]]=stringr::str_replace(temp[i,],".*=","")
               }
               res=cbind(res,as.data.frame(stratalist))

          }
          res

     }
     res
}
