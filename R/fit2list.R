#' Make a list of univariable model with multivariable regression model
#' @param fit An object of class "lm" or "glm"
#' @importFrom purrr map
#' @export
#' @return An object of class "fitlist" which is a list of objects of class "lm" or "glm"
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2list(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2list(fit)
fit2list=function(fit){
     # method = "likelihood"; vanilla = TRUE; threshold = 0.2; digits = NULL

     data=fit2model(fit)

     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     }
     xvars = attr(fit$terms, "term.labels")
     xno = length(xvars)
     yvar = as.character(attr(fit$terms, "variables"))[2]
     xvars
     yvar
     data
     fitlist=map(xvars,function(x){
          myformula=paste0(yvar,"~",x)
          if(mode==1){
               fit=lm(as.formula(myformula),data=data)
          } else if(mode==2) {
               fit=glm(as.formula(myformula),family=family,data=data)
          }
     })
     class(fitlist)="fitlist"
     fitlist

}
