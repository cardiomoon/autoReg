#' Make multivariable regression model by selecting univariable models with p.value below threshold
#' @param fit An object of class "coxph"
#' @param threshold Numeric
#' @examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' fit2multi(fit)
#' @return An object of class "coxph"
#' @export
fit2multi=function(fit,threshold=0.2){
     if(threshold>=1){
          fit
     } else{
          xvars = attr(fit$term, "term.labels")
          add=xvars[which(str_detect(xvars,"strata\\(|cluster\\(|frailty\\("))]
          uni=mycphSimple(fit,threshold=threshold)
          xvars=attr(uni,"sigVars")
          if(length(add)>0) xvars=c(xvars,add)

          if(length(xvars>0)){
               temp = as.character(fit$call)
               dataname = as.character(fit$call)[3]
               f = fit$formula
               y = as.character(f)[2]
               temp4 = paste0(temp[1], "(", y, "~", paste0(xvars, collapse = "+"),
                              ",data=",dataname,")")
               multiModel=eval(parse(text=temp4))
          }
          fit=multiModel
     }
     fit
}


#' Make multivariable regression model by selecting univariable models with p.value below threshold
#' @param fit An object of class "survreg"
#' @param threshold Numeric
#' @examples
#' require(survival)
#' data(cancer)
#' fit=survreg(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
#' survreg2multi(fit)
#' @return An object of class "survreg"
#' @export
survreg2multi=function(fit,threshold=0.2){
     # threshold=0.2
     if(threshold>=1){
          fit
     } else{
          xvars = attr(fit$term, "term.labels")
          add=xvars[which(str_detect(xvars,"strata\\(|cluster\\(|frailty\\("))]
          uni=mysurvregSimple(fit,threshold=threshold)
          xvars=attr(uni,"sigVars")
          if(length(add)>0) xvars=c(xvars,add)

          if(length(xvars>0)){
               temp = as.character(fit$call)
               dataname = as.character(fit$call)[3]
               dataname
               temp=as.character(fit$call)[2]

               temp=strsplit(gsub(" ","",temp),"~")

               y=temp[[1]][1]
               y
               temp4 = paste0("survreg(", y, "~", paste0(xvars, collapse = "+"),
                              ",data=",dataname,",dist='",fit$dist,"')")
               temp4
               multiModel=eval(parse(text=temp4))
          }
          fit=multiModel
     }
     fit
}
