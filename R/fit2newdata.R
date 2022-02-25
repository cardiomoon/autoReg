#' Make a new data of mean value or most frequent value
#' @param fit An object of class "coxph"
#' @param xnames  character Names of explanatory variable to plot
#' @param pred.values A list A list of predictor values
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median logical If TRUE, select median value for numerical variable. Otherwise select most frequent value
#' @param digits integer indicating the number of decimal places
#' @return A data.frame
#' @export
#' @examples
#' require(survival)
#' data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~rx+sex+age,data=colon)
#' fit=coxph(Surv(time,status)~rx+age+strata(sex),data=colon)
#' fit=survreg(Surv(time, status) ~ ph.ecog + age + sex, data=lung, dist="weibull")
#' fit2newdata(fit)
#' fit2newdata(fit,pred.values=list(sex=0,age=58))
#' fit2newdata(fit,pred.values=list(age=c(20,40,60,80),sex=2,ph.ecog=3))
fit2newdata=function(fit,xnames=NULL,pred.values=list(),maxy.lev=5,median=TRUE,digits=1){

     # fit=survreg(Surv(time,status)~age+ph.ecog+sex,data=lung,dist="weibull")
     # pred.values=list(ph.ecog=3,sex=2,age=c(20,40,60,80))
       # xnames=NULL;
       # maxy.lev=5;digits=1;median=TRUE;pred.values=list()
       #fit=coxph(Surv(time,status)~sex*age,data=colon)
     df=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     xvars=xvars[!str_detect(xvars,":")]

     if(length(pred.values)>0){
         if(is.null(xnames)) xnames=names(pred.values)
     } else if(is.null(xnames)) {
             xnames=xvars[1]
     }
     xnames
     tempnames=c()
     no=length(xvars)
     result=list()

     no=length(xnames)
     for(i in seq_along(xnames)){
          if(xnames[i] %in% names(pred.values)){
                  result[[i]]=pred.values[[which(xnames[i] == names(pred.values))]]
          } else if(is.mynumeric(df[[xnames[i]]],maxy.lev=maxy.lev)){
               result[[i]]=unique(fivenum(df[[xnames[i]]])[c(2,3,4)])
          } else{
               result[[i]]=sort(unique(df[[xnames[i]]]))

          }
     }
     df1=expand.grid(result)
     names(df1)=xnames

     suppressMessages(df2<-map2_dfc(names(df1),df1,function(x,y){
          if(length(unique(y))>1){
             paste0(x,"=",y)
          } else{
               NULL
          }
     }))
     labels=apply(df2,1,paste0,collapse=", ")

     add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
     if(length(add)>0){
             xvars=setdiff(xvars,add)
             add=str_remove_all(add,"strata\\(|cluster\\(|frailty\\(|\\)")
             xvars=c(xvars,add)
     }
     xvars=setdiff(xvars,xnames)

     xvars
     for( i in seq_along(xvars)){
          # if(is.mynumeric(df[[xvars[i]]],maxy.lev=maxy.lev)){
          #      result[[no+i]]=round(mean(df[[xvars[i]]],na.rm=TRUE),digits)
          # } else
          if(xvars[i] %in% names(pred.values)){
               result[[no+i]]=pred.values[[which(names(pred.values)==xvars[i])]]
          } else if(is.numeric(df[[xvars[i]]])){
                  if(median) {
                          result[[no+i]]=median(df[[xvars[i]]],na.rm=TRUE)
                  } else{

                       y=table(df[[xvars[i]]])
                       result[[no+i]]=as.numeric(names(y)[which(y==max(y))][1])
                  }

          } else{
               result[[no+i]]=names(which.max(table(df[[xvars[i]]])))[1]
          }
     }
     final=expand.grid(result)
     names(final)=c(xnames,xvars)
     attr(final,"labels")=labels

     final
}

#' Decide whether a vector can be treated as a numeric variable
#'
#' @param x A vector
#' @param maxy.lev An integer indicating the maximum number of unique values of a numeric variable be treated as a categorical variable
#' @return A logical value
#' @examples
#' x=1:5
#' is.mynumeric(x)
#' x=1:13
#' is.mynumeric(x)
#' @export
is.mynumeric=function(x,maxy.lev=5){
     ifelse((is.numeric(x) & (length(unique(x)) > maxy.lev)), TRUE,
            FALSE)
}
