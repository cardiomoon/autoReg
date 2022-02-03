#' Draw an expected plot for a numeric predictor
#'
#' Select cutpoint for a numeric predictor with maxstat.test() and draw survival plot with this cutpoint
#' @param fit An object of class "coxph"
#' @param xname Character Name of explanatory variable to plot
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param se logical Whether or not show se
#' @param ... further arguments to be passed to plot.survfit
#' @return  No return value, called for side effects
#' @importFrom scales hue_pal
#' @importFrom maxstat maxstat.test
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~age+sex,data =colon)
#'expectedPlot2(fit,xname="age")
expectedPlot2=function(fit,xname=NULL,maxy.lev=5,median=TRUE,se=FALSE,...){
           # xnames=c("age");maxy.lev=5;median=TRUE;se=TRUE
     newdata=fit2newdata(fit,xnames=xname,maxy.lev=maxy.lev,median=median)
     newdata
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xname)) xname=xvars[1]

     f = fit$formula
     myt = fit$terms
     y = as.character(f)[2]
     s = paste(y, xname, sep = "~")
mstat = maxstat.test(as.formula(s), data = data, smethod = "LogRank",
                     pmethod = "condMC", B = 300)
cutpoint = mstat$estimate
data$temp = ifelse(data[[xname]] <= cutpoint, 0, 1)

labs = paste(xname, c("\U2264", ">"), cutpoint)
data$temp = factor(data$temp, levels = c(0, 1), labels = labs)

call = paste0(y, "~", "temp")
call
temp=paste0("coxph(",call,",data=data)")
fit=eval(parse(text=temp))


newdata1=data.frame(temp=labs)
newdata1
if(ncol(newdata)>1){
     for(i in 2:ncol(newdata)){
     newdata1[[colnames(newdata)[i]]]=newdata[1,i]
     }
}
newdata1

labels=labs
no=length(labels)
col=scales::hue_pal()(no)
fit1=survfit(fit,newdata=newdata1)
df1=as_tibble(newdata1 %>% select(-all_of("temp")))
df1=df1[1,]
label=map2_chr(names(df1),df1,function(x,y){
     paste0(x,"=",y)
})

label=paste0(label,collapse=", ")
label

     plot(fit1,col=col,lwd=1,conf.int=se,
          main=paste0("Expected Survival by ",paste0(xname,collapse=",")))
     legend("bottomleft",legend=labels,col=col,lwd=2)

     title(sub=label)
}
