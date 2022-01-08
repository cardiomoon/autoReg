#' Add model summary to an object of class gaze
#' @param df An object of class "gaze" or "autoReg"
#' @param fit An object of class "glm" or "lm" or "crr"
#' @param statsname character Name of statistics
#' @return addFitSummary returns an object of \code{\link{gaze}} or \code{\link{autoReg}} - the same class as df
#' @export
#' @examples
#' require(survival)
#' require(dplyr)
#' data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' df=autoReg(fit,uni=FALSE)
#' final=fit2final(fit)
#' df %>% addFitSummary(final,statsname="HR (final)") %>% myft()
addFitSummary=function(df,fit,statsname=""){
     if("crr" %in% class(fit)){
          result=crr2stats(fit)
          result=result[,c(5,6)]
     } else if("imputedReg" %in% class(fit)){
          result=fit[,c(1,11)]
     } else{
          result=fit2summary(fit)
     }
     if(statsname!="") {
          names(result)[names(result)=="stats"]= statsname
     }
     df <-df %>% left_join(result,by="id")
     df[is.na(df)]=""
     df
}
