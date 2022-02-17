#' Draw log-log plot
#' @param fit An object of class "coxph" or "survfit"
#' @param xnames character Names of explanatory variable to plot
#' @param main String Title of plot
#' @param labels String vector Used as legend in legend
#' @param no Numeric The number of groups to be converted
#' @param add.lm logical If true, add linear regression line
#' @param ... Furhter arguments to be passed to plot()
#' @return  A ggplot or no return value, called for side effects
#' @importFrom scales hue_pal
#' @export
#' @examples
#' require(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~x,data=leukemia)
#'loglogplot(fit)
#'fit=survfit(Surv(time,status)~1,data=anderson)
#'loglogplot(fit)
#'fit=survfit(Surv(time,status)~sex,data=anderson)
#'loglogplot(fit)
#'fit=survfit(Surv(time,status)~logWBC,data=anderson)
#'loglogplot(fit)
#'fit=survfit(Surv(time,status)~logWBC+rx,data=anderson)
#'loglogplot(fit,no=2)
loglogplot=function(fit,xnames=NULL,main=NULL,labels=NULL,no=3,add.lm=TRUE,...){
     #xnames=NULL;main=NULL;labels=NULL;no=2
     data=fit2model(fit)
     if("coxph" %in% class(fit)){
         fit=survfit(fit$terms,data=data)
     }
     call=paste0(deparse(fit$call),collapse="")
     temp=c(", data =.*$",".[^\\(]*\\(","^.*=")
     for(i in seq_along(temp)){
          call=sub(temp[i],"",call)
     }
     labels=names(fit$strata)

     if(length(grep(", ",labels[1]))>0){
          xnames=unlist(strsplit(labels[1],", "))

     } else{
          xnames=labels[1]
     }
     xnames=gsub("=.*","",xnames)
     for(i in seq_along(xnames)){
          if(is.mynumeric(data[[xnames[i]]])){

             data=num2factor(data,call=fit$call,xnames[i],no=no)

             fit=survfit(as.formula(call),data=data)
             temp=paste0(xnames[i],"=",xnames[i])
             names(fit$strata)=gsub(temp,xnames[i],names(fit$strata))

          }
     }
     labels=names(fit$strata)
     no=length(labels)
     if(no==0) {
          col=scales::hue_pal()(1)
     } else{
          col=scales::hue_pal()(no)
     }
     if(is.null(main)) {
          if(no>0){
            temp=paste0(xnames,collapse=",")
             main=paste0(paste0("log-log plot by ",temp))
          } else{
               main="log-log plot"
          }
     }
     if(no!=0){
     plot(fit,fun="cloglog",log="x",col=col,lwd=2,...,
          main=main,xlab="Log Time",ylab="Complementary log-log survival")
     if(no>0) legend("topleft",legend=labels,col=col,lwd=2)
     } else{
          y=log(-log(fit$surv))
          x=log(fit$time)
          df=data.frame(x,y)
          p=ggplot(data=df,aes(x=x,y=y))+
               geom_point(alpha=0.5)
          if(add.lm) {
               p=p+stat_smooth(method="lm",formula=y~x)
          }
          p+theme_classic()+
               labs(x="log survival time",y="log-log survival estimate",
                    title="Complementary log-log survival")+
               theme(panel.border=element_rect(fill=NA))
     }
}
