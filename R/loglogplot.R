#' Draw log-log plot
#' @param fit An object of class "coxph"
#' @param xname A character Name of explanatory variable to plot
#' @param main String Title of plot
#' @param labels String vector Used as legend in legend
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @return  No return value, called for side effects
#' @importFrom scales hue_pal
#' @export
#' @examples
#' require(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~x,data=leukemia)
#'loglogplot(fit)
loglogplot=function(fit,xname=NULL,main=NULL,labels=NULL,maxy.lev=5){
     newdata=fit2newdata(fit,xname=xname,maxy.lev=maxy.lev)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xname)) xname=xvars[1]
     if(is.null(labels)) labels=attr(newdata,"labels")
     no=length(labels)
     col=scales::hue_pal()(no)

     fit1=survfit(fit,newdata=newdata)

     if(is.null(main)) {
          main=paste0(paste0("log-log KM curves by ",xname))
     }
     plot(fit1,fun="cloglog",log="x",col=col,lwd=2,
          main=main)
     legend("topleft",legend=labels,col=col,lwd=2)
}
