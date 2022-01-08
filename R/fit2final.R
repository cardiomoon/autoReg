#' Make final model using stepwise backward elimination
#' @param fit An object of class "coxph"
#' @param threshold Numeric
#' @importFrom survival coxph Surv
#' @importFrom stats na.omit
#' @importFrom stringr str_remove_all
#' @return An object of class "coxph" which is described in \code{\link[survival]{coxph}}
#' @examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' final=fit2final(fit)
#' fit2summary(final)
#' @export
fit2final=function(fit,threshold=0.2){

     fit=fit2multi(fit,threshold=threshold)
     dataname = as.character(fit$call)[3]
     data=eval(parse(text=dataname))
     if(sum(is.na(data))>0){
          temp = as.character(fit$call)
          f = fit$formula
          y = as.character(f)[2]
          temp1=str_remove_all(y,"Surv\\(|\\)| ")
          temp1=unlist(strsplit(temp1,","))
          timevar=temp1[1]
          statusvar=temp1[2]
          dataname = as.character(fit$call)[3]
          xvars = attr(fit$term, "term.labels")
          xvars
          add=xvars[which(str_detect(xvars,"strata\\(|cluster\\(|frailty\\("))]
          xvars=setdiff(xvars,add)
          xvars=unique(unlist(map(xvars,~unlist(strsplit(.,":")))))
          xvars2 = c(xvars, timevar, statusvar)
          temp3 = paste0(dataname, "[", paste0("c('",paste0(xvars2,collapse="','"),"')"), "]")
          temp3
          data1 = eval(parse(text = temp3))
          data1 = na.omit(data1)
          if(length(add)>0) xvars=c(xvars,add)
          xvars
          temp4 = paste0(temp[1], "(", y, "~", paste0(xvars, collapse = "+"),
                         ",data=data1)")
          temp4
          fit = eval(parse(text = temp4))
     }
     final = step(fit, direction = "backward", trace = 0)
     final
}
