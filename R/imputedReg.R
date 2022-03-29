#' Make a multiple imputed model
#' @param fit An object of class lm, glm, coxph or survreg
#' @param data a data.frame
#' @param m Number of multiple imputations. The default is m=20.
#' @param seed 	An integer that is used as argument by the set.seed() for offsetting the random number generator.
#' @param digits Integer indicating the number of decimal place
#' @param mode integer indicating summary mode of class survreg
#' @param ... Further argument to be passed to mice
#' @importFrom mice mice pool
#' @importFrom stats as.formula confint glm step
#' @examples
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' imputedReg(fit)
#' \donttest{
#' library(survival)
#' fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' imputedReg(fit)
#' fit=survreg(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' imputedReg(fit)
#' imputedReg(fit,mode=2)
#' }
#' @return An object of class "imputedReg" which inherits from the class "data.frame"
#' @export
imputedReg=function(fit,data=NULL,m=20,seed=1234,digits=2,mode=1,...){

     # fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
     # m=20; seed=1234; digits=2
     #xvars = attr(fit$terms, "term.labels")
     # data(cancer,package="survival")
     # fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
     # data=NULL


     if("survreg" %in% class(fit)) {
          mmode=4
          if(is.null(data)){
               dataname = as.character(fit$call)[3]
               data=get(dataname)
          }
          timevar=attr(fit$y,"dimnames")[[2]][1]
          statusvar=attr(fit$y,"dimnames")[[2]][2]
          xvars = attr(fit$terms, "term.labels")
          formstring=paste0("Surv(",timevar,",",statusvar,")~",paste0(xvars,collapse="+"))
          mydata=data[c(timevar,statusvar,xvars)]
          mydata
     } else if("coxph" %in% class(fit)) {
          mmode=3
          if(is.null(data)){
               dataname = as.character(fit$call)[3]
               data=get(dataname)
          }
          timevar=attr(fit$y,"dimnames")[[2]][1]
          statusvar=attr(fit$y,"dimnames")[[2]][2]
          xvars = attr(fit$terms, "term.labels")
          formstring=paste0("Surv(",timevar,",",statusvar,")~",paste0(xvars,collapse="+"))
          mydata=data[c(timevar,statusvar,xvars)]
          mydata
     } else{
          if("glm" %in% class(fit)) {
               mmode=2
               if(is.null(data)){
                    mydata=fit$data
               } else{
                    mydata=data
               }

          } else {
               mmode=1
               if(is.null(data)){
                    dataname = as.character(fit$call)[3]
                    mydata=get(dataname)
               } else{
                    mydata=data
               }

          }
          xvars=names(fit$model)[-1]
          yvar = as.character(attr(fit$terms, "variables"))[2]
          # str(data)
          # mydata=data[c(xvars,yvar)]
          formstring=paste0(yvar,"~",paste0(attr(fit$terms, "term.labels"),collapse="+"))
     }
     fmt=paste0("%.",digits,"f")

     if(mmode==4){
          tempdf<-mice(mydata,m=m,seed=seed,printFlag=FALSE) %>%
               with(survreg(as.formula(formstring))) %>%
               pool() %>%
               summary(conf.int=TRUE)
          if(mode==1){
               tempdf %>% mutate(
                    ETR=exp(.data$estimate),
                    lower=exp(.data$`2.5 %`),
                    upper=exp(.data$`97.5 %`),
                    stats=paste0(sprintf(fmt,.data$ETR)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) ->df
          } else{
               if(fit$dist=="weibull"){
                    tempdf %>% mutate(
                         HR=exp(-.data$estimate/fit$scale[1]),
                         lower=exp(-.data$`2.5 %`/fit$scale[1]),
                         upper=exp(-.data$`97.5 %`/fit$scale[1]),
                         stats=paste0(sprintf(fmt,.data$HR)," (",
                                      sprintf(fmt,.data$lower),"-",
                                      sprintf(fmt,.data$upper),", ",
                                      p2character2(.data$p.value),")")
                    ) ->df
                    # if(length(fit$scale)>1){
                    #      for(i in 2:length(fit$scale)){
                    #           df$HR[df$id==names(fit$scale)[i]]=exp(-df$Value[df$id==names(fit$scale)[i]]/fit$scale[i])
                    #      }
                    # }
               } else if(fit$dist=="exponential"){
                    tempdf %>% mutate(
                         HR=exp(-.data$estimate),
                         lower=exp(-.data$`2.5 %`),
                         upper=exp(-.data$`97.5 %`),
                         stats=paste0(sprintf(fmt,.data$HR)," (",
                                      sprintf(fmt,.data$lower),"-",
                                      sprintf(fmt,.data$upper),", ",
                                      p2character2(.data$p.value),")")
                    ) ->df
               } else if(fit$dist=="loglogistic"){
                    tempdf %>% mutate(
                         OR=exp(-.data$estimate/fit$scale),
                         lower=exp(-.data$`2.5 %`/fit$scale),
                         upper=exp(-.data$`97.5 %`/fit$scale),
                         stats=paste0(sprintf(fmt,.data$OR)," (",
                                      sprintf(fmt,.data$lower),"-",
                                      sprintf(fmt,.data$upper),", ",
                                      p2character2(.data$p.value),")")
                    ) ->df
               }
          }

     } else if(mmode==3){
          mice(mydata,m=m,seed=seed,printFlag=FALSE,...) %>%
               with(coxph(as.formula(formstring))) %>%
               pool() %>%
               summary(conf.int=TRUE) %>%
               mutate(
                    HR=exp(.data$estimate),
                    lower=exp(.data$`2.5 %`),
                    upper=exp(.data$`97.5 %`),
                    stats=paste0(sprintf(fmt,.data$HR)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) ->df
     } else if(mmode==2){
          mice(mydata,m=m,seed=seed,printFlag=FALSE,...) %>%
               with(glm(as.formula(formstring),family=fit$family$family)) %>%
               pool() %>%
               summary(conf.int=TRUE) %>%
               mutate(
                    OR=exp(.data$estimate),
                    lower=exp(.data$`2.5 %`),
                    upper=exp(.data$`97.5 %`),
                    stats=paste0(sprintf(fmt,.data$OR)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) ->df
     } else{
          mice(mydata,m=m,seed=seed,printFlag=FALSE) %>%
               with(lm(as.formula(formstring))) %>%
               pool() %>% summary(conf.int=TRUE) %>%
               mutate(
                    lower=.data$`2.5 %`,
                    upper=.data$`97.5 %`,
                    stats=paste0(sprintf(fmt,.data$estimate)," (",
                                 sprintf(fmt,.data$lower)," to ",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) -> df
          df<-df %>% rename(Estimate=.data$estimate)
     }
     df<-df %>%rename(id=.data$term)
     class(df)=c("imputedReg","data.frame")
     df
}
