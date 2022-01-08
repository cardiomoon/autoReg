#'Perform univariable and multivariable regression and stepwise backward regression automatically
#'@param x An object of class lm, glm or coxph
#'@param ... Further arguments
#' @return autoReg returns an object of class "autoReg" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#' \item{attr(*,"model")}{name of model. One of "lm","glm" or "coxph"}
#'}
#'@examples
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' autoReg(fit)
#' autoReg(fit,uni=FALSE,final=TRUE)
#' autoReg(fit,uni=FALSE,imputed=TRUE)
#' fit=lm(mpg~wt*hp+am+I(wt^2),data=mtcars)
#' autoReg(fit,final=TRUE)
#' autoReg(fit,imputed=TRUE)
#'@export
autoReg=function(x,...)  UseMethod("autoReg")


#'@describeIn autoReg S3 method for a class lm
#'@export
autoReg.lm=function(x,...){
     autoReg_sub(fit=x,...)
}

#'@describeIn autoReg S3 method for a class glm
#'@export
autoReg.glm=function(x,...){
     autoReg_sub(fit=x,...)
}

#'Perform univariable and multivariable regression and stepwise backward regression automatically
#'@param fit An object of class lm or glm
#'@param threshold numeric
#'@param uni logical whether or not perform univariate regression
#'@param multi logical whether or not perform multivariate regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not include imputed model
#'@param keepstats logical whether or not keep statistics
#'@param showstats logical whether or not show descriptive statistics
#'@param ... Further arguments to be passed to imputedReg()
#' @importFrom stringr str_detect fixed
#' @importFrom purrr reduce map_dfr
#' @importFrom dplyr left_join bind_rows all_of
#' @importFrom rlang .data
#' @return An object of class "autoReg" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#' \item{attr(*,"model")}{name of model. One of "lm","glm" or "coxph"}
#'}
#' @export
autoReg_sub=function(fit,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,imputed=FALSE,keepstats=FALSE,showstats=TRUE,...){
          # fit=lm(mpg~wt*hp+am+disp,data=mtcars)
          # fit=lm(Sepal.Length~Sepal.Width*Species,data=iris)
      # fit=glm(cens~horTh*progrec+pnodes,data=GBSG2,family="binomial")
                                 # threshold=0.2;uni=FALSE;multi=TRUE;final=FALSE;imputed=TRUE;keepstats=TRUE
     xvars = attr(fit$terms, "term.labels")
     yvar = as.character(attr(fit$terms, "variables"))[2]
     # xvars
     # yvar
     data1=fit2model(fit)
     data1
     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     }
     if(uni==FALSE) threshold=1
     result=getSigVars(fit,threshold=threshold,final=final)
     result
     xvars
     others=setdiff(xvars,names(data1))
     others
     xvars=setdiff(xvars,others)
     if(length(xvars)>0){
     formula=paste0(yvar,"~",paste0(xvars,collapse="+"))
     formula
     if(any(str_detect(names(data1),fixed("I(")))){
         temp=names(data1)[str_detect(names(data1),fixed("I("))]
         data1<-data1 %>% select(-all_of(temp))
     }
     df=gaze(x=as.formula(formula),data=data1,show.n=keepstats,show.p=FALSE)
     df=as.data.frame(df)
     } else{
       df=data.frame(name="",desc="",id="")
       df=df[-1,]
       df
     }
     df

     others
     if(length(others)>0){

          for(i in 1:length(others)){
               # i=1
               name=others[i]
               desc="others"
               if(str_detect(name,":")) {
                    desc="interaction"
                    temp=getInteraction(name,data=data1)
                    temp$n=NULL

               } else if(str_detect(name,fixed("I("))){
                    desc="interpretation"
                    temp=data.frame(name=name,desc=desc,id=name)
               }
               df
               temp
               class(df)="data.frame"
               df=bind_rows(df,temp)
          }
     }
     df
     if(length(which(duplicated(df$id)))>0){
        df=df[-which(duplicated(df$id)),]
     }
     dflist=list()
     dflist[[1]]=df
     stat=ifelse(mode==1,"Coefficient","OR")
     if(uni){
          fit
          fitlist=fit2list(fit)
          if(keepstats){
              df1=map_dfr(fitlist,fit2stats) %>%
                  filter(.data$id!="(Intercept)")
              df1$mode="univariate"
          } else{
            df1=fit2summary(fitlist)
            names(df1)[2]=paste(stat,"(univariable)")
          }

          dflist[["uni"]]=df1
     }
     if(multi){
          formula2=paste0(yvar,"~",paste0(result$sigVars,collapse="+"))
          if(mode==1){
               fit2=lm(as.formula(formula2),data=data1)
          } else if(mode==2){
               fit2=glm(as.formula(formula2),data=data1,family=family)
          }
          if(keepstats){
              df2=fit2stats(fit2)
              df2$mode="multivariate"
          } else{
             df2=fit2summary(fit2)
             names(df2)[2]=paste(stat,"(multivariable)")
          }
          dflist[["multi"]]=df2
          df2
          if(imputed & (!final)){
             df4=imputedReg(fit2,data=data1,...)

             # fit2
             # as.character(fit2$call)[3]
                    # df4=imputedReg(fit2)

             if(keepstats) {
                 if(mode==2){
                 df4=df4 %>%
                     select(.data$OR,.data$lower,.data$upper,
                                    .data$p.value,.data$id,.data$stats) %>%
                     mutate(mode="imputed") %>%
                     rename(p=.data$p.value)
                 } else{
                 df4=df4 %>%
                   select(.data$Estimate,.data$lower,.data$upper,
                          .data$p.value,.data$id,.data$stats) %>%
                   mutate(mode="imputed") %>%
                   rename(p=.data$p.value)
                 }
             } else{
                 df4 = df4 %>% select(.data$id,.data$stats)
                 temp=paste0(ifelse(mode==1,"Coefficients ","OR "),"(imputed)")
                 names(df4)[2]=temp
             }
             dflist[["imputed"]]=df4
          }
     }
     if(final) {
          formula3=paste0(yvar,"~",paste0(result$finalVars,collapse="+"))
          if(mode==1){
               fit3=lm(as.formula(formula3),data=data1)
          } else if(mode==2){
               fit3=glm(as.formula(formula3),data=data1,family=family)
          }
          if(keepstats){
              df3=fit2stats(fit3)
              df3$mode="final"
          } else{
              df3=fit2summary(fit3)
              names(df3)[2]=paste(stat,"(final)")
          }


          dflist[["final"]]=df3
          if(imputed){

             df4=imputedReg(fit3,data=data1,...)
             if(keepstats) {
                 df4=df4 %>%
                     select(.data$OR,.data$lower,.data$upper,
                            .data$p.value,.data$id,.data$stats) %>%
                     mutate(mode="imputed") %>%
                     rename(p=.data$p.value)
             } else{
                df4 = df4 %>% select(.data$id,.data$stats)
                temp=paste0(ifelse(mode==1,"Coefficients ","OR "),"(imputed)")
                names(df4)[2]=temp
             }
             dflist[["imputed"]]=df4
          }
     }
     if(keepstats){

         Final=reduce(dflist[-1],bind_rows)
         Final

     } else{
        Final=reduce(dflist,left_join,by="id")
        # names(Final)[1]=paste0("Dependent: ",yvar)
        # names(Final)[2]=" "

        Final
     }
     class(Final)=c("autoReg","data.frame")
     Final[is.na(Final)]=""
     attr(Final,"model")=ifelse(mode==1,"lm","glm")
     if(is.null(attr(data1[[yvar]],"label"))){
       attr(Final,"yvars")=yvar
     } else{
       attr(Final,"yvars")=attr(data1[[yvar]],"label")
     }

     Final

}

