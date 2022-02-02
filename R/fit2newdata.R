#' Make a new data of mean value or most frequent value
#' @param fit An object of class "coxph"
#' @param xnames  character Names of explanatory variable to plot
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
#' fit2newdata(fit)
fit2newdata=function(fit,xnames=NULL,maxy.lev=5,median=TRUE,digits=1){

       #  xnames=NULL;maxy.lev=5;digits=1
       # fit=coxph(Surv(time,status)~sex*age,data=colon)
     df=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")

     xvars=xvars[!str_detect(xvars,":")]
     if(is.null(xnames)) {
             xnames=xvars[1]
     }
     tempnames=c()
     no=length(xvars)
     result=list()

     no=length(xnames)
     for(i in seq_along(xnames)){
     if(is.mynumeric(df[[xnames[i]]],maxy.lev=maxy.lev)){
          result[[i]]=fivenum(df[[xnames[i]]])[c(1,3,5)]

     } else{
          result[[i]]=sort(unique(df[[xnames[i]]]))

     }
     }
     df1=expand.grid(result)
     names(df1)=xnames

     suppressMessages(df2<-map2_dfc(names(df1),df1,function(x,y){
             paste0(x,"=",y)
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
          if(is.numeric(df[[xvars[i]]])){
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
