#' Make a residual plot of NULL model
#'
#' @param x An object of calss coxph
#' @param add.log logical If true, log of predictor varaibles are added
#' @param type character type of residuals
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
#' @importFrom survival coxph Surv
#' @importFrom ggplot2 ggplot geom_point stat_smooth facet_wrap theme_classic
#' @examples
#' library(survival)
#' data(pharmacoSmoking,package="asaur")
#' pharmacoSmoking$priorAttemptsT=pharmacoSmoking$priorAttempts
#' pharmacoSmoking$priorAttemptsT[pharmacoSmoking$priorAttemptsT>20]=20
#' x=coxph(Surv(ttr,relapse)~age+priorAttemptsT+longestNoSmoke,data=pharmacoSmoking)
#' residualNull(x)
residualNull=function(x,add.log=TRUE,type="martingale"){
     #add.log=TRUE;type="martingale"
     xvars=attr(x$terms,"term.labels")
     yvar=attr(attr(x$terms,"dataClasses"),"names")[1]
     data=fit2model(x)

     result=c()
     xvars=map_chr(xvars,~ifelse(is.mynumeric(data[[.]]),.,NULL))

     if(add.log){
     suppressMessages(temp<-purrr::map_dfc(xvars,function(x){
          log(data[[x]])
     }))
     names(temp)=paste0("log(",xvars,")")
     data=cbind(data,temp)
     }

     fit0=coxph(as.formula(paste0(yvar,"~1")),data=data)
     rr=residuals(fit0,type=type)
     data=cbind(data,rr)
     data=data[3:ncol(data)]
     longdf=pivot_longer(data,cols=-.data$rr)
     longdf$name=factor(longdf$name,levels=unlist(map(xvars,~c(.,paste0("log(",.,")")))))
     ggplot(longdf,aes_string(x="value",y="rr"))+
          geom_point(alpha=0.5)+
          stat_smooth(method="loess",formula=y~x)+
          theme_classic()+
          labs(y=paste(type,"residuals"),x="")+
          facet_wrap(~name,ncol=2,scales="free")
}
