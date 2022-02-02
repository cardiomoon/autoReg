#' Extract survival data from an object of class "survfit"
#' @param fit An object of class "survfit"
#' @importFrom purrr map_dfc
#' @return A data.frame
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#'fit=survfit(Surv(time,status)~rx+sex+age,data=colon)
#'survfit2df(fit)
survfit2df=function(fit){
     cols=c("time","n.risk","n.event","n.censor","surv","std.err","upper","lower")
     suppressMessages(res<-map_dfc(cols,~fit[[.]]))
     names(res)=cols
     if(!is.null(fit$strata)){
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
     }
     res
}
