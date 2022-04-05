#'@describeIn autoReg S3 method for a class survreg
#'@export
autoReg.survreg=function(x,...){
     autoRegsurvreg(x,...)
}

#' perform automatic regression for a class of survreg
#'@param x An object of class survreg
#'@param threshold numeric
#'@param uni logical whether or not perform univariable regression
#'@param multi logical whether or not perform multivariable regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not perform multiple imputation
#'@param keepstats logical whether or not keep statistic
#'@param mode integer
#'@param ... Further arguments to be passed to gaze()
#'@examples
#' require(survival)
#' require(dplyr)
#' data(cancer)
#' fit=survreg(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' autoReg(fit)
#' autoReg(fit,uni=TRUE,threshold=1)
#' autoReg(fit,uni=TRUE,final=TRUE)
#' autoReg(fit,uni=TRUE,final=TRUE) %>% myft()
#' \dontrun{
#' autoReg(fit,mode=2)
#' autoReg(fit,uni=TRUE,threshold=1,,mode=2)
#' autoReg(fit,uni=TRUE,final=TRUE,mode=2)
#' autoReg(fit,uni=TRUE,final=TRUE,mode=2) %>% myft()
#' autoReg(fit,final=TRUE,imputed=TRUE) %>% myft()
#' autoReg(fit,final=TRUE,imputed=TRUE,mode=2) %>% myft()
#' }
#' @return autoRegsurvreg returns an object of class "autoReg" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#' \item{attr(*,"model")}{name of model. One of "lm","glm","coxph" or "survreg"}
#'}
#' @export
autoRegsurvreg=function(x,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,imputed=FALSE,keepstats=FALSE,mode=1,...){
        # x=survreg(Surv(time,status)~rx+age+sex+obstruct+perfor,data=colon)
        #   threshold=0.2;uni=TRUE;multi=TRUE;final=TRUE;imputed=FALSE;keepstats=FALSE;mode=1
     if(uni==FALSE) threshold=1
     fit=x
     # dataname = as.character(fit$call)[3]
     # if(missing(data)) {
     #      data=eval(parse(text=dataname))
     # }
     # fit
     data=fit2model(fit)
     temp=as.character(fit$call)[2]
     temp=strsplit(gsub(" ","",temp),"~")
     y=temp[[1]][1]
     y
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
          df=mysurvregSimple(fit,threshold=threshold,mode=mode)
          df
          if(keepstats){
               df=df[c(1:5,13)]
               df$mode="univariable"
          } else{

               df=df[c(1,13)]
               if(mode==1){
                    df= rename(df, "ETR (univariable)"=.data$stats)
               } else{
                    df= rename(df, "HR (univariable)"=.data$stats)
               }
          }
          mylist[[no]]=df
          no=no+1
     }
     if(multi){
          fit=survreg2multi(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(fit,mode=mode)
               df$mode="multivariable"
          } else{
               df=fit2summary(fit,mode=mode)
               if(mode==1){
                    df=rename(df,"ETR (multivariable)"=.data$stats)
               } else{
               df=rename(df,"HR (multivariable)"=.data$stats)
               }
          }
          mylist[[no]]=df
          no=no+1
     }
     if(final){
          final1=survreg2final(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(final1,mode=mode)
               df$mode="final"
          } else{

               df=fit2summary(final1,mode=mode)
               if(mode==1){
                    df=rename(df,"ETR (final)"=.data$stats)
               } else{
               df=rename(df,"HR (final)"=.data$stats)
               }
          }
          mylist[[no]]=df
          no=no+1
     }
     if(imputed){

          imputed=imputedReg(fit,mode=mode)

          if(keepstats){
               df=imputed[c(ifelse(mode==1,"ETR",ifelse(fit$dist=="loglogistic","OR","HR")),"lower","upper","p.value","id","stats")] %>%
                    rename("p"=.data$p.value)
               df$mode="imputed"
          } else{
               df=imputed[c("id","stats")]
               if(mode==1){
                    df=rename(df,"ETR (imputed)"=.data$stats)
               } else if(fit$dist!="loglogistic"){
                    df=rename(df,"HR (imputed)"=.data$stats)
               } else{
                    df=rename(df,"OR (imputed)"=.data$stats)
               }
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
     attr(Final,"model")="survreg"
     temp=summary(fit)$logtest
     # attr(Final,"lik")=paste0("n=",fit$n,", events=",fit$nevent,
     #                          ", Likelihood ratio test=",format(round(temp[1], 2))," on ",temp[2]," df(",
     #                          p2character2(temp[3],add.p=TRUE),")")

     Final


}
