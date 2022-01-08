#' Get explanatory variables of a model with significance level below the threshold
#' @param fit An object of class lm or glm
#' @param threshold Numeric
#' @param final logical if true, perform stepwise regression using step()
#' @importFrom purrr map2 map
#' @export
#' @return A list containing the following components:
#' \describe{
#'   \item{sigVars}{names of explanatory variables which have significant levels below the threshold in univariable model}
#'   \item{finalVars}{names of explanatory variables included in final model as a result of \code{\link[stats]{step}}}
#' }
#' @examples
#' library(survival)
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' getSigVars(fit)
#' fit=lm(mpg~hp*wt+am,data=mtcars)
#' getSigVars(fit)
getSigVars=function(fit,threshold=0.2,final=TRUE){

     # fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
     # threshold=0.2;final=TRUE
     xvars = attr(fit$terms, "term.labels")
     yvar = as.character(attr(fit$terms, "variables"))[2]
     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     }

     fitlist=fit2list(fit)

     sigVars<-map2(fitlist,xvars,function(x,y){
          if(any(summary(x)$coeff[-1, 4] < threshold)) {
               return(y)
          } else{
               return(NULL)
          }
     }) %>% unlist()
     sigVars
     finalVars=c()

     if(final &(length(sigVars)>0)){
          myformula=paste0(yvar,"~",paste0(sigVars,collapse="+"))
          myformula
          data=fit$model
          if(mode==1){
               fit=lm(as.formula(myformula),data=data)
          } else if(mode==2){
               fit=glm(as.formula(myformula),family=family,data=data)
          }
          final=step(fit,trace=0)
          finalVars=attr(final$terms, "term.labels")
     }
     list(sigVars=sigVars,finalVars=finalVars)
}
