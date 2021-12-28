#'Bootstrap simulation for model prediction
#'
#'Generate model predictions against a specified set of explanatory levels with bootstrapped confidence intervals.
#'@param fit An object of class lm or glm
#'@param newdata A data.frame
#'@param R Number of simulations. Note default R=100 is very low.
#'@param type he type of prediction required, see predict.glm. The default for glm models is on the scale of the response variable.
#'Thus for a binomial model the default predictions are predicted probabilities.
#'@param ... Further arguments to be passed to boot::boot
#'@importFrom boot boot
#'@importFrom broom tidy
#'@importFrom stats predict
#'@importFrom utils combn
#'@export
#'@examples
#'data(GBSG2,package="TH.data")
#'fit=glm(cens~horTh+pnodes,data=GBSG2,family="binomial")
#'newdata=expand.grid(horTh=factor(c(1,2),labels=c("no","yes")),pnodes=1:51)
#'bootPredict(fit,newdata)
#'library(survival)
#'fit=coxph(Surv(time,cens)~age+horTh+progrec+pnodes,data=GBSG2)
bootPredict=function(fit, newdata,R=100,type="response",...){

     data=fit$model
     formula=fit$formula
     if("glm" %in% class(fit)){
          mode=2
          family=fit$family$family
     } else if("lm" %in% class(fit)){
          mode=1
     }

     bs=function(formula,data,indices){
          d = data[indices,]
          if(mode==1){
               fit=lm(formula,data=d)
          } else if(mode==2){
               fit=glm(formula,family="binomial",data=d)
          }
          out=predict(fit,newdata=newdata,type=type)
          return(out)
     }

     bs.out<-boot(data=data,statistic=bs,R=R,formula=fit$terms,...)
     bs.tidy = broom::tidy(bs.out, conf.int = TRUE, conf.level = 0.95, conf.method = "perc")
     bs.tidy = data.frame(bs.tidy)
     df.out = bs.tidy[, c(1, 4, 5)]

     colnames(df.out) = c("estimate","lower","upper")
     df=cbind(newdata,df.out)
     df
}

