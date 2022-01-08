#'@describeIn autoReg S3 method for a class coxph
#'@export
autoReg.coxph=function(x,...){
     autoRegCox(x,...)
}

#' perform automatic regression for a class of coxph
#'@param x An object of class coxph
#'@param threshold numeric
#'@param uni logical whether or not perform univariable regression
#'@param multi logical whether or not perform multivariable regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not perform multiple imputation
#'@param keepstats logical whether or not keep statistic
#'@param ... Further arguments to be passed to gaze()
#'@examples
#' require(survival)
#' require(dplyr)
#' data(cancer)
#' fit=coxph(Surv(time,status==2)~log(bili)+age+cluster(edema),data=pbc)
#' autoReg(fit)
#' fit=coxph(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
#' autoReg(fit)
#' autoReg(fit,uni=TRUE,threshold=1)
#' autoReg(fit,uni=TRUE,final=TRUE) %>% myft()
#' data(colon_s,package="finalfit")
#' fit=coxph(Surv(time,status)~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s)
#' autoReg(fit,uni=TRUE,threshold=1)
#' autoReg(fit,uni=TRUE,imputed=TRUE)
#' @return autoRegCox returns an object of class "autoReg" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#' \item{attr(*,"model")}{name of model. One of "lm","glm" or "coxph"}
#'}
#' @export
autoRegCox=function(x,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,imputed=FALSE,keepstats=FALSE,...){
     # x=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon);data=colon
     # threshold=0.2;uni=TRUE;multi=TRUE;final=FALSE;imputed=FALSE;keepstats=FALSE
     if(uni==FALSE) threshold=1
     fit=x
     # dataname = as.character(fit$call)[3]
     # if(missing(data)) {
     #      data=eval(parse(text=dataname))
     # }
     # fit
     data=fit2model(fit)
     f = fit$formula
     y = as.character(f)[2]
     temp1=str_remove_all(y,"Surv\\(|\\)| ")
     temp1=unlist(strsplit(temp1,","))
     timevar=temp1[1]
     statusvar=temp1[2]
     xvars = attr(fit$terms, "term.labels")
     xvars
     xvars
     add=xvars[str_detect(xvars,"strata\\(|frailty\\(")]
     add
     if(str_detect(paste0(deparse(fit$call),collapse=""),"cluster")){
          temp=paste0(deparse(fit$call),collapse="")
          temp=unlist(strsplit(temp,"cluster"))[2]
          temp
          add=c(add,paste0("cluster=",str_remove_all(temp,"=|\\)| ")))
          add
     }
     myformula=paste0("~",paste0(xvars,collapse="+"))
     myformula
     mylist=list()
     mylist[[1]]=gaze(as.formula(myformula),data=data,...)
     # mylist[[1]]=gaze(as.formula(myformula),data=data)
     # names(mylist[[1]])[1:3]=c(paste0("Dependent: Surv(",timevar,",",statusvar,")")," ","all")
     # names(mylist[[1]])[1:3]
     mylist[[1]]
     no=2
     if(uni){
          df=mycphSimple(fit,threshold=threshold)
          if(keepstats){
               df=df[c(2:4,7:9)]
               df$mode="univariable"
          } else{
               df=df[c(8:9)] %>%
                    rename("HR (univariable)"=.data$stats)
          }
          mylist[[no]]=df
          no=no+1
     }
     if(multi){
          fit=fit2multi(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(fit)
               df$mode="multivariable"
          } else{
               df=fit2summary(fit) %>%
                    rename("HR (multivariable)"=.data$stats)
          }
          mylist[[no]]=df
          no=no+1
     }
     if(final){
          final1=fit2final(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(final1)
               df$mode="final"
          } else{
               df=fit2summary(final1) %>%
                    rename("HR (final)"=.data$stats)
          }
          mylist[[no]]=df
          no=no+1
     }
     if(imputed){
          imputed=imputedReg(fit)

          if(keepstats){
               df=imputed[c("HR","lower","upper","p.value","id","stats")] %>%
                    rename("p"=.data$p.value)
               df$mode="imputed"
          } else{
               df=imputed[c("id","stats")] %>%
                    rename("HR (imputed)"=.data$stats)
          }
          mylist[[no]]=df

     }
     if(keepstats){

          Final=reduce(mylist[-1],bind_rows)
          Final

     } else{
          Final=reduce(mylist,left_join,by="id")
          # names(Final)[1]=paste0("Dependent: Suv(",timevar,",",statusvar,")")
          # names(Final)[2]=" "
          Final
     }
     class(Final)=c("autoReg","data.frame")
     Final[is.na(Final)]=""
     if(length(add)>0) {
          attr(Final,"add")=add
          if(str_detect(add,"frailty")) {
               if(final) {
                    no=which(str_detect(rownames(summary(final1)$coefficients),"frailty"))
                    p=data.frame(summary(final1)$coef)$p[no]
                    attr(Final,"add")=paste(add,p2character2(p,add.p = TRUE),summary(final1)$print2)

               } else{
                    no=which(str_detect(rownames(summary(fit)$coefficients),"frailty"))
                    p=data.frame(summary(fit)$coef)$p[no]
                    attr(Final,"add")=paste(add,p2character2(p,add.p = TRUE),summary(fit)$print2)
               }
          }
     }
     attr(Final,"yvars")=attr(attr(fit$terms,"dataClasses"),"names")[1]
     attr(Final,"model")="coxph"
     Final

}
