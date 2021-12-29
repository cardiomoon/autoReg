#' Make a list of univariable model with multivariable regression model
#' @param fit An object of class lm or glm
#' @param data A data.frame or NULL
#' @importFrom purrr map
#' @export
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2list(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2list(fit)
fit2list=function(fit,data=NULL){
     # method = "likelihood"; vanilla = TRUE; threshold = 0.2; digits = NULL

     if(is.null(data)){
         if("glm" %in% class(fit)) {
                 data=fit$data %>% dplyr::select(names(fit$model))
         } else if("lm" %in% class(fit)){
             data=fit$model
         } else if("glmerMod" %in% class(fit)){
             data=fit@frame
         }

     }

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

#' Get explanatory variables of a model with significance level below the threshold
#' @param fit An object of class lm or glm
#' @param threshold Numeric
#' @param final logical if true, perform stepwise regression using step()
#' @importFrom purrr map2 map
#' @export
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


#' Summarise statistics with a model or model list
#' @param fit An object of class lm or glm of filtlist
#' @param ... Further argument to be passed to fit2stats
#' @export
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2summary(fit)
#' fitlist=fit2list(fit)
#' fit2summary(fitlist)
#' @importFrom dplyr filter
fit2summary=function(fit,...){

     if("fitlist" %in% class(fit)){
          df=map_dfr(fit,fit2stats,...)
          df %>% filter(.data$id!="(Intercept)") -> df
     } else{
          df=fit2stats(fit,...)
     }
     df %>% select(.data$id,.data$stats)
}


#' Summarise statistics with a model
#' @param fit An object of class lm or glm
#' @param method character choices are one of the c("likelihood","wald")
#' @param digits integer indicating the number of decimal places
#' @importFrom moonBook extractOR extractHR
#' @export
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2stats(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2stats(fit)
fit2stats=function(fit,method="likelihood",digits=2){
     # method="likelihood";digits=2
    mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     } else if("glmerMod" %in% class(fit)){
         mode=3
     } else if("coxph" %in% class(fit)){
         mode=4
     }
     mode
     fmt=paste0("%.",digits,"f")
     if(mode==4){
         result=extractHR(fit)
         names(result)[2:3]=c("lower","upper")
         result$id=rownames(result)
         result$stats=paste0(sprintf(fmt,result$HR)," (",
                             sprintf(fmt,result$lower),"-",
                             sprintf(fmt,result$upper),", ",p2character2(result$p),")")
         df=result
         df
     } else if(mode>1){
          result=extractOR(fit, method = method,digits=digits)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$OR)," (",
                    sprintf(fmt,result$lower),"-",
                    sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
          df
     } else if(mode==1){
          result=base::cbind(summary(fit)$coefficients,confint(fit))
          temp=round(result[,c(1,5,6)],digits)
          id=rownames(result)
          stats=paste0(
               sprintf(fmt,temp[,1]),"(",
               sprintf(fmt,temp[,2])," to ",
               sprintf(fmt,temp[,3]),", ",p2character2(result[,4]),")")
          df=data.frame(id=id,Estimate=result[,1],lower=result[,5],upper=result[,6],stats=stats)
     }
     df
}

#'Change p value to string
#'@param x a numeric
#'@param digits interger indicating decimal place
#'@param add.p logical
#'@export
p2character2=function(x,digits=3,add.p=TRUE){
     cut = 1/(10^digits)
     temp = sprintf(paste0("%.", digits, "f"), x)
     temp = stringr::str_replace(temp, "^0", "")
     temp
     for (i in 1:length(x)) {
          if (is.na(x[i])) {
               temp[i] = ""
          }
          else if (x[i] < cut) {
               temp[i] = stringr::str_replace(temp[i], "0$", "1")
               if(add.p){
                  temp[i] = paste0("p<", temp[i])
               } else{
                  temp[i] = paste0("<", temp[i])
               }
          } else{
               if(add.p) {
                    temp[i]=paste0("p=",temp[i])
               } else{
                    temp[i]
               }
          }
     }
     temp
}


#'Perform univariable and multivariable regression and stepwise backward regression automatically
#'@param x An object of class lm, glm or coxph
#'@param ... Further arguments
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
#'@param data A data.frame or NULL
#'@param threshold numeric
#'@param uni logical whether or not perform univariate regression
#'@param multi logical whether or not perform multivariate regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not include imputed model
#'@param keepstats logical whether or not keep statistics
#' @importFrom stringr str_detect fixed
#' @importFrom purrr reduce map_dfr
#' @importFrom dplyr left_join bind_rows all_of
#' @importFrom rlang .data
#' @export
autoReg_sub=function(fit,data=NULL,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,imputed=FALSE,keepstats=FALSE){
          #fit=lm(mpg~wt*hp+I(wt^2)+am,data=mtcars)
          #fit=lm(Sepal.Width~Sepal.Length*Species,data=iris)
      # fit=glm(cens~horTh*progrec+pnodes,data=GBSG2,family="binomial")
            # data=NULL;threshold=0.2;uni=FALSE;multi=TRUE;final=FALSE;imputed=FALSE;keepstats=FALSE
     xvars = attr(fit$terms, "term.labels")
     yvar = as.character(attr(fit$terms, "variables"))[2]

     if(is.null(data)){
         if("glm" %in% class(fit)) {
             if(uni==TRUE) {
                 data=fit$data %>% dplyr::select(names(fit$model))
             }else{
                 data=fit$model
             }
         } else if("lm" %in% class(fit)){
             data=fit$model
         } else if("glmerMod" %in% class(fit)){
             data=fit@frame
         }

     }
     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     }
     if(uni==FALSE) threshold=1
     result=getSigVars(fit,threshold=threshold,final=final)
     result
     formula=paste0(yvar,"~.")
     if(any(str_detect(names(data),fixed("I(")))){
         temp=names(data)[str_detect(names(data),fixed("I("))]
         data<-data %>% select(-all_of(temp))
     }
     df=gaze(x=as.formula(formula),data=data,show.n=keepstats)
     df=as.data.frame(df)
     df
     others=setdiff(xvars,names(data))
     others
     if(length(others)>0){

          for(i in 1:length(others)){
               # i=1
               name=others[i]
               desc="others"
               if(str_detect(name,":")) {
                    desc="interaction"
                    temp=getInteraction(name,data=data)
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
               fit2=lm(as.formula(formula2),data=data)
          } else if(mode==2){
               fit2=glm(as.formula(formula2),data=data,family=family)
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
             df4=imputedReg(fit2)
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
     if(final) {
          formula3=paste0(yvar,"~",paste0(result$finalVars,collapse="+"))
          if(mode==1){
               fit3=lm(as.formula(formula3),data=data)
          } else if(mode==2){
               fit3=glm(as.formula(formula3),data=data,family=family)
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

             df4=imputedReg(fit3)
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
        names(Final)[1]=paste0("Dependent: ",yvar)
        names(Final)[2]=" "

        Final
     }
     class(Final)=c("autoReg","data.frame")
     Final[is.na(Final)]=""
     Final

}

#' S3 method print for an object of class autoReg
#' @param x An object of class autoReg
#' @param ... Further arguments
#' @examples
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' autoReg(fit)
#' @export
print.autoReg=function(x,...){
    printdf(x)
    if(!is.null(attr(x,"add"))) cat(attr(x,"add"),"\n")
}


#'Print function for data.frame
#'@param x A data.frame
#'@param showid logical if TRUE, show id
printdf=function(x,showid=FALSE){

     if(("autoReg" %in% class(x))&(showid==FALSE)) x$id=NULL
    lengths1=map_int(x,maxnchar)
    lengths2=map_int(names(x),maxnchar)
    lengths=pmax(lengths1,lengths2)+2
    lineno=sum(lengths)
    no=ncol(x)
    side=rep("both",no)
    list(names(x),lengths,side) %>% pmap_chr(str_pad) -> header
    drawline(lineno);cat("\n")
    cat(paste0(header,collapse=""),"\n")
    drawline(lineno);cat("\n")
    if("imputedReg" %in% class(x)) {
        side=c(rep("right",1),rep("left",no-1))
    } else {
        side=c(rep("right",2),rep("left",no-2))
    }
    list(x,lengths,side) %>% pmap_dfc(str_pad) ->x1
    for(i in 1:nrow(x)){
        cat(paste0(x1[i,],collapse=""),"\n")
    }
    drawline(lineno);cat("\n")
}


#' Make a multiple imputed model
#' @param fit An object of class lm or glm
#' @param data a data.frame
#' @param m Number of multiple imputations. The default is m=5.
#' @param seed 	An integer that is used as argument by the set.seed() for offsetting the random number generator.
#' @param digits Integer indicating the number of decimal place
#' @param ... Further argument to be passed to mice
#' @importFrom mice mice pool
#' @importFrom stats as.formula confint glm step
#' @examples
#' data(cancer,package="survival")
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' imputedReg(fit)
#' \donttest{
#' library(survival)
#' fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' imputedReg(fit)
#' }
#' @export
imputedReg=function(fit,data=NULL,m=20,seed=1234,digits=2,...){

     #fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
        # data=NULL;m=20; seed=1234; digits=2
     #xvars = attr(fit$terms, "term.labels")
        # data(cancer,package="survival")
        # fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
        # data=NULL


     if("coxph" %in% class(fit)) {
          mode=3
          dataname = as.character(fit$call)[3]
          if(is.null(data)) {
               data=eval(parse(text=dataname))
          }
          timevar=attr(fit$y,"dimnames")[[2]][1]
          statusvar=attr(fit$y,"dimnames")[[2]][2]
          xvars = attr(fit$terms, "term.labels")
          formstring=paste0("Surv(",timevar,",",statusvar,")~",paste0(xvars,collapse="+"))
          mydata=data[c(timevar,statusvar,xvars)]
          mydata
     } else{
          if("glm" %in% class(fit)) {
               mode=2
               if(is.null(data)) data=fit$data

          } else {
               mode=1
               if(is.null(data)) data=fit$model

          }
          xvars=names(fit$model)[-1]
          yvar = as.character(attr(fit$terms, "variables"))[2]
          mydata=data[c(xvars,yvar)]
          formstring=paste0(yvar,"~",paste0(attr(fit$terms, "term.labels"),collapse="+"))
     }
     fmt=paste0("%.",digits,"f")
     if(mode==3){
          mice(mydata,m=m,seed=seed,printFlag=FALSE,...) %>%
               with(coxph(as.formula(formstring))) %>%
               pool() %>%
               summary(conf.int=TRUE) %>%
               mutate(
                    HR=exp(.data$estimate),
                    lower=exp(.data$`2.5 %`),
                    upper=exp(.data$`97.5 %`),
                    stats=paste0(sprintf(fmt,.data$HR)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) ->df
     } else if(mode==2){
          mice(mydata,m=m,seed=seed,printFlag=FALSE,...) %>%
               with(glm(as.formula(formstring),family=fit$family$family)) %>%
               pool() %>%
               summary(conf.int=TRUE) %>%
               mutate(
                    OR=exp(.data$estimate),
                    lower=exp(.data$`2.5 %`),
                    upper=exp(.data$`97.5 %`),
                    stats=paste0(sprintf(fmt,.data$OR)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) ->df
     } else{
          mice(mydata,m=m,seed=seed,printFlag=FALSE) %>%
               with(lm(as.formula(formstring))) %>%
               pool() %>% summary(conf.int=TRUE) %>%
               mutate(
                    lower=.data$`2.5 %`,
                    upper=.data$`97.5 %`,
                    stats=paste0(sprintf(fmt,.data$estimate)," (",
                                 sprintf(fmt,.data$lower),"-",
                                 sprintf(fmt,.data$upper),", ",
                                 p2character2(.data$p.value),")")
               ) -> df
     }
     df<-df %>%rename(id=.data$term)
     class(df)=c("imputedReg","data.frame")
     df
}


