#' Summarize statistics with a model or model list
#' @param fit An object of class "lm" or "glm" or "fitlist" which is a result of \code{\link{fit2list}}
#' @param mode integer
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
#' fit=survreg(Surv(time,status)~rx+sex+age+obstruct+nodes,data=colon)
#' fit2summary(fit)
#' @importFrom dplyr filter
fit2summary=function(fit,mode=1,...){

     if("fitlist" %in% class(fit)){
          df=map_dfr(fit,fit2stats,...)
          df %>% filter(.data$id!="(Intercept)") -> df
     } else{
          df=fit2stats(fit,mode=mode,...)
           # df=fit2stats(fit,mode=mode)
     }
     if("survreg" %in% class(fit)){
          colnames(df)[1]="id"
          class(df)="data.frame"
     }
     df %>% select(.data$id,.data$stats)
}


#' Summarize statistics with a model
#' @param fit An object of class lm or glm or coxph or survreg
#' @param method character choices are one of the c("likelihood","wald")
#' @param digits integer indicating the number of decimal places
#' @param mode integer
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
#' fit=survreg(Surv(time,status)~rx+sex+age+obstruct+nodes,data=colon)
#' fit2stats(fit)
fit2stats=function(fit,method="default",digits=2,mode=1){
      # method="default";digits=2;mode=1
     cmode=1
     if("glm" %in% attr(fit,"class")) {
          cmode=2
          family = fit$family$family
     } else if("glmerMod" %in% class(fit)){
          cmode=3
     } else if("coxph" %in% class(fit)){
          cmode=4
     } else if("survreg" %in% class(fit)){
          cmode=5
     }
     cmode
     fmt=paste0("%.",digits,"f")
     if(cmode==5){
       df=gaze(fit)
       df=df[df$LB!="NA",]
       addp=function(x){
            result=c()
            for(i in seq_along(x)){
                 if(substr(x[i],1,1)=="<") {
                      temp=paste0("p",x[i])
                 } else{
                      temp=paste0("p=",x[i])
                 }
                 result=c(result,temp)
            }
            result
       }
       df$p1=addp(df$p)
       if(mode==1){
            df$stats=paste0(sprintf(fmt,as.numeric(df$ETR))," (",
                                sprintf(fmt,as.numeric(df$LB)),"-",
                                sprintf(fmt,as.numeric(df$UB)),", ",df$p1,")")

            df
       } else{
            df$stats=paste0(sprintf(fmt,as.numeric(df$HR))," (",
                                sprintf(fmt,as.numeric(df$lower)),"-",
                                sprintf(fmt,as.numeric(df$upper)),", ",df$p1,")")

            df
       }
     } else if(cmode==4){
          result=extractHR(fit)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$HR)," (",
                              sprintf(fmt,result$lower),"-",
                              sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
          df
     } else if(cmode>1){
          result=extractOR(fit, method = method,digits=digits)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$OR)," (",
                              sprintf(fmt,result$lower),"-",
                              sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
          df
     } else if(cmode==1){
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
