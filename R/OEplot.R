#' Draw an Observed vs Expected plot
#' @param fit An object of class "coxph"
#' @param xname A character Name of explanatory variable to plot
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @return  No return value, called for side effects
#' @importFrom dplyr semi_join as_tibble
#' @importFrom graphics legend lines
#' @importFrom scales hue_pal
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~rx,data=colon)
#'OEplot(fit)
OEplot=function(fit,xname=NULL,maxy.lev=5){
     #     xname="grp";maxy.lev=5
     newdata=fit2newdata(fit,xname=xname,maxy.lev=maxy.lev)

     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xname)) xname=xvars[1]
     labels=attr(newdata,"labels")
     no=length(labels)
     col=scales::hue_pal()(no)
     fit1=survfit(fit,newdata=newdata)
     plot(fit1,lty=2,col=col,lwd=1,
          main=paste0("Observed versus Expected Plot by ",xname))
     legend("bottomleft",legend=labels,col=col,lwd=2)
     fit2=survfit(fit$terms,data=data)
     survdata=survfit2df(fit2)
     newdata[]=lapply(newdata,as.character)
     newdata1=as_tibble(newdata)
     for(i in 1:nrow(newdata)){
          df=semi_join(survdata,newdata1[i,],by=names(newdata1))
          lines(df$time,df$surv,col=col[i],type="s",lwd=2)
     }
     legend("topright",legend=c("Expected","Observed"),lty=c(2,1),lwd=c(1,2))

}


