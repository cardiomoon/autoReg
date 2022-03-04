#'@describeIn gaze default S3 method
#'@export
#'@examples
#'library(survival)
#'x=coxph(Surv(time,status) ~rx,data=anderson1)
#'gaze(x)
#'x=coxph(Surv(time,status) ~rx*logWBC,data=anderson1)
#'gaze(x)
gaze.coxph=function(x,...){
     df=as.data.frame(summary(x)$coef)
     df=df[-2]
     df1=as.data.frame(summary(x)$conf.int)
     df$HR=df1[[1]]
     df$lower=df1[[3]]
     df$upper=df1[[4]]
     df$var=rownames(summary(x)$coef)
     df
     df<-df %>% dplyr::select(.data$var,everything())
     attr(df,"call")=gsub(" ","",paste0(deparse(x$call),collapse=""))
     attr(df,"yvars")=attr(attr(x$terms,"dataClasses"),"names")[1]
     attr(df,"model")="coxph"
     attr(df,"lik")=fit2lik(x)
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     myformat(df)
}

#'@describeIn gaze default S3 method
#'@export
#'@examples
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' gaze(x)
#' x=survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), lung)
#' gaze(x)
gaze.survreg=function(x,...){
     df=as.data.frame(summary(x)$table)
     df$id=rownames(df)
     df1=as.data.frame(confint(x))
     df1$id=rownames(df1)
     df=left_join(df,df1,by="id")
     names(df)[6:7]=c("lower","upper")
     df<-df %>% dplyr::select(.data$id,everything())
     attr(df,"call")=gsub(" ","",paste0(deparse(x$call),collapse=""))
     attr(df,"yvars")=attr(attr(x$terms,"dataClasses"),"names")[1]
     attr(df,"model")="survreg"
     attr(df,"lik")=fit2lik(x)
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     myformat(df)
}

#' extract likelihood information with a coxph object
#' @param x An object of class "coxph" or "survreg"
#' @importFrom stats pchisq
#' @return A string
#' @export
#' @examples
#' library(survival)
#' fit=coxph(Surv(time,status) ~rx,data=anderson)
#' fit2lik(fit)
fit2lik=function(x){

     if("survreg" %in% class(x)){
          pdig <- max(1, getOption("digits") - 4)
          nobs <- length(x$linear)
          chi <- 2 * diff(x$loglik)
          df <- sum(x$df) - x$idf
          temp=summary(x)$parms
          temp=paste(temp,"\nLoglik(model)=", format(round(x$loglik[2], 1)), "  Loglik(intercept only)=",
              format(round(x$loglik[1], 1)))
          if (df > 0)
               temp=paste(temp,"\n\tChisq=", format(round(chi, 2)), "on",
                          round(df, 1), "degrees of freedom, p=", format.pval(pchisq(chi, df, lower.tail = FALSE), digits = pdig), "\n")
          else temp=paste(temp,"\n")
          omit <- x$na.action
          if (length(omit))
               temp=paste(temp,"n=", nobs, " (", naprint(omit), ")\n", sep = "")
          else temp=paste(temp,"n=", nobs, "\n")
          temp
     } else{

     temp=summary(x)$logtest
     temp1=paste0("n=",x$n,", events=",x$nevent,
                  ", Likelihood ratio test=",format(round(temp[1], 2))," on ",temp[2]," df (",
                  p2character2(temp[3],add.p=TRUE),")")
     temp1
     }
}


#'@describeIn gaze default S3 method
#'@export
#'@importFrom stats naprint
#'@examples
#'data(cancer,package="survival")
#'fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#'gaze(fit)
gaze.glm=function(x,...){
     df=as.data.frame(summary(x)$coef)
     df1=fit2stats(x,digits=3)
     df$OR=df1$OR
     df$lower=df1$lower
     df$upper=df1$upper
     df$id=df1$id
     df<-df %>% dplyr::select(.data$id,everything())

     attr(df,"yvars")=attr(attr(x$terms,"dataClasses"),"names")[1]
     attr(df,"model")="glm"
     temp=paste0("Null deviance: ",round(x$null.deviance,1),"(df: ",x$df.null,
                 "), Residual deviance: ",round(x$deviance,1),"(df: ",x$df.residual,
                 ") AIC:", round(x$aic,1) )
     if (nzchar(mess <- naprint(x$na.action))) temp=paste0(temp,"\n  (", mess, ")")
     temp=paste0(temp,"\nNumber of Fisher Scoring iterations: ", x$iter)
     attr(df,"dev")=temp
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     myformat(df)
}

#'@describeIn gaze default S3 method
#'@export
#'@importFrom stats naprint pf
#'@examples
#'fit=lm(mpg~wt*hp+am+I(wt^2),data=mtcars)
#'gaze(fit)
gaze.lm=function(x,...){
     df=as.data.frame(summary(x)$coef)
     df1=fit2stats(x,digits=3)
     df$lower=df1$lower
     df$upper=df1$upper
     df$id=df1$id
     df<-df %>% dplyr::select(.data$id,everything())

     attr(df,"yvars")=attr(attr(x$terms,"dataClasses"),"names")[1]
     attr(df,"model")="lm"

     x=summary(x)

     temp=paste0("Residual SE: ", round(x$sigma,3), " on ", x$df[2], " DF")
     if (nzchar(mess <- naprint(x$na.action))) {temp=paste0(temp,"\n  (", mess, ")")}
     temp=paste0(temp,", Multiple R^2: ", round(x$r.squared, digits = 4))
     temp=paste0(temp,", Adjusted R^2: ", round(x$adj.r.squared,4),
                 "\nF-statistic: ", round(x$fstatistic[1L],2), " on ", x$fstatistic[2L], " and ",
                 x$fstatistic[3L], " DF,  p-value: ", format.pval(pf(x$fstatistic[1L],
                                                                     x$fstatistic[2L], x$fstatistic[3L], lower.tail = FALSE),
                                                                  digits = 4))
     attr(df,"add")=temp
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     myformat(df)
}

#'@describeIn gaze default S3 method
#'@export
#'@examples
#' data(melanoma,package="boot")
#' melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#' fit=crrFormula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
#' gaze(fit)
gaze.tidycrr=function(x,...){

     df1=x$tidy[,c(1:5)]
     names(df1)=c("var","coef","se(coef)","z","p")
     df2=crr2stats(x)
     df=cbind(df1,df2[,c(1:3)])
     df
     df$p=p2character2(df$p,add.p=FALSE)
     df[]=lapply(df,function(x) {
          if(is.numeric(x)) {
               x=round(x,3)
          }else{
               x
          }
     })
     df
     attr(df,"model")="crr"
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     myformat(df)
}
