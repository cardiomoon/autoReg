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
