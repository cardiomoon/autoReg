#' Make a new data of mean value or most frequent value
#' @param fit An object of class "coxph"
#' @param xname A character Name of explanatory variable to plot
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @return A data.frame
#' @export
#' @examples
#' require(survival)
#' data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~rx+sex+age,data=colon)
#' fit2newdata(fit)
fit2newdata=function(fit,xname=NULL,maxy.lev=5){

     df=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")

     if(is.null(xname)) xname=xvars[1]
     tempnames=c()
     no=length(xvars)
     result=list()

     if(is.mynumeric(df[[xname]],maxy.lev=maxy.lev)){
          result[[1]]=fivenum(df[[xname]])[c(1,3,5)]

     } else{
          result[[1]]=sort(unique(df[[xname]]))

     }

     labels=paste0(xname,"=",result[[1]])
     xvars=setdiff(xvars,xname)

     for( i in seq_along(xvars)){
          if(is.mynumeric(df[[xvars[i]]],maxy.lev=maxy.lev)){
               result[[1+i]]=mean(df[[xvars[i]]])
          } else if(is.numeric(df[[xvars[i]]])){
               y=table(df[[xvars[i]]])
               result[[i+1]]=as.numeric(names(y)[which(y==max(y))][1])
          } else{
               result[[i+i]]=names(which.max(table(df[[xvars[i]]])))[1]
          }
     }
     final=expand.grid(result)
     names(final)=c(xname,xvars)
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
