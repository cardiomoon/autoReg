#'@describeIn gaze default S3 method
#'@importFrom tibble as_tibble
#'@export
#'@examples
#'library(survival)
#'x=coxph(Surv(time,status) ~rx,data=anderson1)
#'gaze(x)
#'x=coxph(Surv(time,status) ~rx*logWBC,data=anderson1)
#'gaze(x)
gaze.coxph=function(x,...){
     df=as_tibble(summary(x)$coef)
     df=df[-2]
     df
     df1=as_tibble(summary(x)$conf.int)
     df$HR=df1[[1]]
     df$lower=df1[[3]]
     df$upper=df1[[4]]
     df$var=rownames(summary(x)$coef)
     df
     df<-df %>% dplyr::select(.data$var,everything())

     attr(df,"yvars")=attr(attr(x$terms,"dataClasses"),"names")[1]
     attr(df,"model")="coxph"
     temp=summary(x)$logtest
     temp1=paste0("n=",x$n,", events=",x$nevent,
                              ", Likelihood ratio test=",format(round(temp[1], 2))," on ",temp[2]," df (",
                              p2character2(temp[3],add.p=TRUE),")")
     attr(df,"lik")=temp1
     attr(df,"summary")=TRUE
     class(df)=c("autoReg","data.frame")
     df
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
     df
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
     df
}
