#' Summarize statistics with a model or model list
#' @param fit An object of class "lm" or "glm" or "fitlist" which is a result of \code{\link{fit2list}}
#' @param ... Further argument to be passed to fit2stats
#' @export
#' @return An object of class "data.frame"
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2summary(fit)
#' fitlist=fit2list(fit)
#' fit2summary(fitlist)
#' @importFrom dplyr filter
fit2summary=function(fit,...){

     if("fitlist" %in% class(fit)){
          df=map_dfr(fit,fit2stats,...)
          df %>% filter(.data$id!="(Intercept)") -> df
     } else{
          df=fit2stats(fit,...)
     }
     df %>% select(.data$id,.data$stats)
}


#' Summarize statistics with a model
#' @param fit An object of class lm or glm
#' @param method character choices are one of the c("likelihood","wald")
#' @param digits integer indicating the number of decimal places
#' @importFrom moonBook extractOR extractHR
#' @export
#' @return An object of class "data.frame"
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2stats(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2stats(fit)
fit2stats=function(fit,method="default",digits=2){
     # method="likelihood";digits=2
     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     } else if("glmerMod" %in% class(fit)){
          mode=3
     } else if("coxph" %in% class(fit)){
          mode=4
     }
     mode
     fmt=paste0("%.",digits,"f")
     if(mode==4){
          result=extractHR(fit)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$HR)," (",
                              sprintf(fmt,result$lower),"-",
                              sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
          df
     } else if(mode>1){
          result=extractOR(fit, method = method,digits=digits)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$OR)," (",
                              sprintf(fmt,result$lower),"-",
                              sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
          df
     } else if(mode==1){
          result=base::cbind(summary(fit)$coefficients,confint(fit))
          temp=round(result[,c(1,5,6)],digits)
          id=rownames(result)
          stats=paste0(
               sprintf(fmt,temp[,1])," (",
               sprintf(fmt,temp[,2])," to ",
               sprintf(fmt,temp[,3]),", ",p2character2(result[,4]),")")
          df=data.frame(id=id,Estimate=result[,1],lower=result[,5],upper=result[,6],stats=stats)
     }
     df
}
