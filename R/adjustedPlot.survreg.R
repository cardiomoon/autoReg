#'Draw predicted survival curve with an object survreg
#'
#' @param x An object of class survreg
#' @param xnames Character Names of explanatory variable to plot
#' @param pred.values A list A list of predictor values
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param newdata A data.frame or NULL
#' @param addCox  logical Whether or not add KM
#' @return No return value, called for side effects
#' @examples
#' library(survival)
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' adjustedPlot(x)
#' adjustedPlot(x,addCox=TRUE)
#' x=survreg(Surv(time, status) ~ sex, data=lung,dist="weibull")
#' adjustedPlot(x,addCox=TRUE)
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' adjustedPlot(x)
#' x=survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), lung)
#' adjustedPlot(x)
#' adjustedPlot(x,addCox=TRUE)
adjustedPlot.survreg=function(x,xnames=NULL,pred.values=NULL,maxy.lev=5,median=TRUE,
                              newdata=NULL,addCox=FALSE){

     #xnames=NULL;pred.values=NULL;maxy.lev=5;median=TRUE
     #newdata=NULL;addCox=FALSE
     fit=x
     xvars = attr(fit$terms, "term.labels")
     if(length(xvars)>0){
          if(is.null(xnames)) xnames=xvars[1]
          if(is.null(newdata)){
               newdata=fit2newdata(fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev,median=median)
               labels=attr(newdata,"labels")
          }
          df1=as_tibble(newdata %>% select(-all_of(xnames)))
          df1=df1[1,]
          label=map2_chr(names(df1),df1,function(x,y){
               paste0(x,"=",y)
          })
          label=paste0(label,collapse=", ")
          no=length(labels)

     } else{
          no=1
     }
     if(addCox){
          fitcox=coxph(as.formula(deparse(fit$terms)),data=fit2model(fit))
          if(length(xvars)>0){
               fit1=survfit(fitcox,newdata=newdata)
          }  else {
               labels=""
               fit1=survfit(fitcox)
          }
          coxdf=survfit2df(fit1,labels=labels)
          coxdf

     }


     if(no==1) {
          col="black"
     } else{
          col=scales::hue_pal()(no)
     }

     p = seq(0.01, 0.99, by=.01)

     if(length(xvars)>0){
          ptime <- predict(fit, newdata=newdata, type='quantile',
                           p = p, se=TRUE)

     } else{
          ptime <- predict(fit,  type='quantile',
                           p =p, se=TRUE)
          ptime$fit=ptime$fit[1,]
          ptime$se.fit=ptime$se.fit[1,]
          label=""
     }

     if("matrix" %in% class(ptime$fit)){
          x=as_tibble(t(ptime$fit),.name_repair="minimal")
     } else{
          x=as_tibble(ptime$fit,.name_repair="minimal")
     }
     names(x)=paste0("group",1:ncol(x))
     x
     y=1-p
     df1=cbind(y,x)
     df1

     if(addCox){
          plot(fit1,col=col,lty=2,xlab="Survival Time",ylab="Survival Probability",conf.int=FALSE,sub=label)


          for(i in 1:no){lines(df1[,i+1],y,col=col[i]) }

          if(no==1){
               legends=c(fit$dist,"Cox")
          } else{
               legends=c(paste0(labels,",",fit$dist),paste0(labels,",Cox"))
          }
          lty=c(rep(1,no),rep(2,no))

          legend(x = "topright",
                 legend = legends,
                 lty=c(rep(1,no),rep(2,no)),
                 col = rep(col,no),
                 cex=0.8)


     } else{
          df1
          plot(df1[,2],y,col=col[1],type="l",
               xlab="Survival Time",ylab="Survival Probability",sub=label)

          if(no>1) for(i in 2:no){lines(df1[,i+1],y,col=col[i]) }
          if(no==1) labels=fit$dist
          legend(x = "topright",
                 legend = labels,
                 lwd=1,
                 col = col,
                 cex=0.8)
     }


}