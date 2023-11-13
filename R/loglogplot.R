#' Draw log-log plot
#' @param fit An object of class "coxph" or "survfit"
#' @param xnames character Names of explanatory variable to plot
#' @param main String Title of plot
#' @param labels String vector Used as legend in legend
#' @param no Numeric The number of groups to be converted
#' @param add.loess logical If true, add loess regression line
#' @param add.lm logical If true, add linear regression line
#' @param type character "l" or "p"
#' @param se logical If true, add se
#' @param what character One of c("surv","survOdds","failureOdds")
#' @param legend.position legend position. One of c("left","top","bottom","right") or numeric vector of length 2.
#' @param ... Furhter arguments to be passed to plot()
#' @return  A ggplot or no return value, called for side effects
#' @importFrom scales hue_pal
#' @importFrom ggplot2 ggplot
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
#'loglogplot(fit,no=2)
#'fit=survfit(Surv(time,status)~logWBC+rx,data=anderson)
#'loglogplot(fit,no=2)
#'fit=survfit(Surv(time,status)~rx,data=anderson)
#'loglogplot(fit,type="p")
#'fit=survfit(Surv(time,status)~WBCCAT,data=anderson2)
#'loglogplot(fit,type="p",what="survOdds")
#'loglogplot(fit,type="p",what="failureOdds")
loglogplot=function(fit,xnames=NULL,main=NULL,labels=NULL,no=3,
                    add.loess=FALSE,add.lm=TRUE,type="l",se=TRUE,what="surv",
                    legend.position=NULL,...){
       # xnames=NULL;main=NULL;labels=NULL;no=2
       # add.loess=FALSE;add.lm=TRUE;type="l";se=TRUE;what="surv";legend.position=NULL
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

     if(no==0){
          y=log(-log(fit$surv))
          x=log(fit$time)
          df=data.frame(x,y)
          p=ggplot(data=df,aes_string(x="x",y="y"))+
               geom_point(alpha=0.5)
          if(add.loess) { p=p+stat_smooth(method="loess",formula=y~x,se=se,color="blue") }
          if(add.lm) { p=p+stat_smooth(method="lm",formula=y~x,se=se,color="red") }
          p=p+theme_classic()+
               labs(x="log survival time",y="log-log survival estimate",
                    title="Complementary log-log survival")+
               theme(panel.border=element_rect(fill=NA))
          p
     } else if(type=="l"){
          plot(fit,fun="cloglog",log="x",col=col,lwd=2,...,
               main=main,xlab="Log Time",ylab="Complementary log-log survival")
          if(no>0) legend("topleft",legend=labels,col=col,lwd=2)
     } else{
          df=survfit2df(fit)
          if(what=="surv"){
               df$y=log(-log(df$surv))
               if(is.null(legend.position)) legend.position=c(0.2,0.85)
               ylab="log-log survival estimate"
          } else if(what=="survOdds"){
               df$y=log(df$surv/(1-df$surv))
               if(is.null(legend.position)) legend.position=c(0.8,0.85)
               ylab="Survival Odds"
          } else{
               df$y=log((1-df$surv)/df$surv)
               if(is.null(legend.position)) legend.position=c(0.2,0.85)
               ylab="Failure Odds"
          }
          df$x=log(df$time)
          df
          #df$strata=gsub("\U2264","<=",df$strata)

          p=ggplot(df,aes_string(x="x",y="y",color="strata"))+
               geom_point()+scale_color_discrete(label=label_parse)
          if(add.loess) p=p+stat_smooth(aes_string(fill="strata"),method="loess",formula=y~x,se=se,alpha=0.2)
          if(add.lm) p=p+stat_smooth(aes_string(fill="strata"),method="lm",formula=y~x,se=se,alpha=0.2)
          p=p+ labs(x="log survival time",y=ylab)+
               theme_classic()+
               theme(panel.border=element_rect(fill=NA),
                     legend.position=legend.position,
                     legend.title=element_blank()
                     )
          p

     }
}
