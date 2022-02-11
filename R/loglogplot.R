#' Draw log-log plot
#' @param fit An object of class "coxph" or "survfit"
#' @param xnames character Names of explanatory variable to plot
#' @param main String Title of plot
#' @param labels String vector Used as legend in legend
#' @return  No return value, called for side effects
#' @importFrom scales hue_pal
#' @export
#' @examples
#' require(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~x,data=leukemia)
#'fit=survfit(Surv(time,status)~sex,data=anderson)
#'loglogplot(fit)
loglogplot=function(fit,xnames=NULL,main=NULL,labels=NULL){

     if("coxph" %in% class(fit)){
          data=fit2model(fit)
          fit=survfit(fit$terms,data=data)
     }
     labels=names(fit$strata)
     no=length(labels)
     col=scales::hue_pal()(no)


     if(is.null(main)) {
          temp=unlist(strsplit(labels[1],","))
          temp=gsub("=.*$","",temp)
          temp=paste0(temp,collapse=",")
          main=paste0(paste0("log-log plot by ",temp))
     }
     plot(fit,fun="cloglog",log="x",col=col,lwd=2,
          main=main,xlab="Log Time",ylab="Complementary log-log survival")
     legend("topleft",legend=labels,col=col,lwd=2)
}
