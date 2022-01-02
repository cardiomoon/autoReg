#' Restore fit model data containing AsIs expressions
#' @param fit An object of class lm, glm or coxph
#' @return a data.frame
#' @examples
#' require(survival)
#' pbc$status2=ifelse(pbc$status==2,1,0)
#' fit=coxph(Surv(time,status2)~age+log(bili),data=pbc)
#' fit2model(fit)
#' @importFrom predict3d restoreData restoreData2 restoreData3
#' @importFrom stringr str_remove_all
#' @export
fit2model=function(fit){

     if("coxph" %in% class(fit)){
          dataname = as.character(fit$call)[3]
          data=eval(parse(text=dataname))
          f = fit$formula
          y = as.character(f)[2]
          temp1=str_remove_all(y,"Surv\\(|\\)| ")
          temp1=unlist(strsplit(temp1,","))
          timevar=temp1[1]
          statusvar=temp1[2]
          xvars = attr(fit$terms, "term.labels")
          xvars
          timevar

          if(str_detect(statusvar,"==")) {
               statusvar=unlist(strsplit(statusvar,"=="))[1]
          }
          add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
          if(length(add)>0){
               xvars=setdiff(xvars,add)
               add=str_remove_all(add,"strata\\(|cluster\\(|frailty\\(|\\)")
               xvars=c(xvars,add)
          }
          myformula=paste0(timevar,"~",paste0(c(statusvar,xvars),collapse="+"))
          myformula
          fit0=lm(myformula,data=data)
          modelData=fit0$model
     } else if("glmerMod" %in% class(fit)){
          modelData=fit@frame
          data=modelData
     } else if("glm" %in% class(fit)){

          y = as.character(fit$formula)[2]
          y

          if(str_detect(y,"==")) {
               dataname = as.character(fit$call)[3]
               data=eval(parse(text=dataname))
               f = fit$formula
               y = as.character(f)[2]
               y
               xvars = attr(fit$terms, "term.labels")
               xvars
               if(str_detect(y,"==")) {
                    temp=unlist(strsplit(y,"=="))[1]
                    temp=str_replace_all(temp," ","")
                    xvars=c(xvars,temp)
               }
               add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
               if(length(add)>0){
                    xvars=setdiff(xvars,add)
                    add=str_remove_all(add,"strata\\(|cluster\\(|frailty\\(|\\)")
                    xvars=c(xvars,add)
               }
               myformula=paste0(y,"~",paste0(xvars,collapse="+"))
               myformula
               fit0=lm(myformula,data=data)
               modelData=fit0$model
          } else{
               modelData=fit$model
          }
          data=modelData
     } else{
          modelData=fit$model
          data=modelData
     }
     modelData %>%
          restoreData() %>%
          restoreData2() %>%
          restoreData3() -> df
     df


}
