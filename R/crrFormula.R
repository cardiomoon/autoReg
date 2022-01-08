#' Competing Risk Regression with Formula
#' @param x formula time+status~explanatory variables
#' @param data data a data.frame
#' @param ... Further arguments to be passed to \code{\link[cmprsk]{crr}}
#' @importFrom cmprsk crr
#' @importFrom stats model.matrix
#' @return An object of class "crr" which is described in \code{\link[cmprsk]{crr}}
#' @examples
#' data(melanoma,package="boot")
#' melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#' crrFormula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
#' @export
crrFormula=function(x,data,...){
     f=x
     myt=terms(f,data=data)
     xvars=attr(myt,"term.labels")
     temp=strsplit(deparse(x),"~")[[1]][1]
     temp=gsub(" ","",temp)
     yvars=strsplit(temp,"+",fixed=TRUE)[[1]]

     if(length(yvars)!=2) {
          cat("The formula should be : time+status~(explanatory variables)\n")
          return(NULL)
     } else if(length(xvars)<1){
          cat("The formula should be : time+status~(explanatory variables).\n")
          cat("The explanatory variables should be more than one.\n")
          return(NULL)
     }
     timevar=yvars[1]
     statusvar=yvars[2]
     formula=paste0("~",paste0(xvars,collapse="+"))
     cov=model.matrix(as.formula(formula),data=data)[,-1]
     cmprsk::crr(data[[timevar]],data[[statusvar]],cov,...)
}


#' Extract statistics from an object of class crr
#' @param x an object of class crr
#' @param digits integer indication the position of decimal place
#' @examples
#' data(melanoma,package="boot")
#' melanoma$status_crr=ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#' x=crrFormula(time+status_crr~age+sex+thickness+ulcer,data=melanoma)
#' crr2stats(x)
#' @export
crr2stats=function(x,digits=2){

     df=as.data.frame(cbind(summary(x)$conf.int,summary(x)$coef[,5]))
     df$id=rownames(df)
     df=df[-2]
     names(df)=c("HR","lower","upper","p","id")
     fmt=paste0("%.",digits,"f")
     df$stats=paste0(sprintf(fmt,df$HR)," (",
                     sprintf(fmt,df$lower),"-",
                     sprintf(fmt,df$upper),", ",p2character2(df$p),")")
     df
}

