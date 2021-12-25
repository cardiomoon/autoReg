#' Make a list of univariable model with multivariable regression model
#' @param fit An object of class lm or glm
#' @importFrom purrr map
#' @export
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2list(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2list(fit)
fit2list=function(fit){
     # method = "likelihood"; vanilla = TRUE; threshold = 0.2; digits = NULL
     data=fit$model

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
#' @export
#' @examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' fit2stats(fit)
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' fit2stats(fit)
fit2stats=function(fit,method="likelihood",digits=2){
     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          family = fit$family$family
     }
     fmt=paste0("%.",digits,"f")
     if(mode==2){
          result=extractOR2(fit, method = method,digits=digits)
          names(result)[2:3]=c("lower","upper")
          result$id=rownames(result)
          result$stats=paste0(sprintf(fmt,result$OR)," (",
                    sprintf(fmt,result$lower),"-",
                    sprintf(fmt,result$upper),", ",p2character2(result$p),")")
          df=result
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

#'Extract Odds Ratio
#'@param x	An object of class glm
#'@param method	character choices are one of the c("likelihood","wald")
#'@param digits	integer indicating the number of decimal places
#'@importFrom stats coef confint.default
#'@examples
#'data(cancer,package="survival")
#'x=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#'extractOR2(x)
#'@export
extractOR2=function (x, method = "likelihood", digits = 4)
{
    if (method == "likelihood") {
        suppressMessages(a <- confint(x))
    }
    else {
        a <- confint.default(x)
    }
    if (length(x$coef) == 1) {
        result = c(exp(coef(x)), exp(a))
        result = round(result, digits)
        result = c(result, round(summary(x)$coefficient[, 4],
                                 4))
        temp = names(result)[1]
        res = data.frame(result)
        result = t(res)
        result = data.frame(result)
        rownames(result)[1] = temp
    }
    else {
        result = data.frame(exp(coef(x)), exp(a))
        result = round(result, digits)
        result = cbind(result, round(summary(x)$coefficient[,
                                                            4], 4))
    }
    colnames(result) = c("OR", "lcl", "ucl", "p")
    result
}

#'Perform univariable and multivariable regression and stepwise backward regression automatically
#'@param fit An object of class lm or glm
#'@param threshold numeric
#'@param uni logical whether or not perform univariate regression
#'@param multi logical whether or not perform multivariate regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not include imputed model
#'@examples
#' library(survival)
#' data(cancer)
#' fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
#' autoReg(fit)
#' autoReg(fit,uni=FALSE,final=TRUE)
#' autoReg(fit,uni=FALSE,imputed=TRUE)
#' fit=lm(mpg~wt*hp+am+I(wt^2),data=mtcars)
#' autoReg(fit,final=TRUE)
#' autoReg(fit,imputed=TRUE)
#' @importFrom stringr str_detect fixed
#' @importFrom purrr reduce map_dfr
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#' @export
autoReg=function(fit,threshold=0.2,uni=TRUE,multi=TRUE,final=FALSE,imputed=FALSE){
         # fit=lm(mpg~wt*hp+am,data=mtcars)
          #threshold=0.2;uni=TRUE;multi=TRUE;final=TRUE;imputed=TRUE
     xvars = attr(fit$terms, "term.labels")
     yvar = as.character(attr(fit$terms, "variables"))[2]
     fit$model

     data=fit$model
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
         data<-data %>% select(-temp)
     }
     df=mySummary(x=as.formula(formula),data=data,keepid=TRUE)
     df=as.data.frame(df)
     df
     others=setdiff(xvars,names(data))
     others
     if(length(others)>0){
          for(i in 1:length(others)){

               name=others[i]
               desc="others"
               if(str_detect(name,":")) {
                    desc="interaction"
               } else if(str_detect(name,fixed("I("))){
                    desc="interpretation"
               }
               temp=data.frame(name=name,desc=desc,unit="",value="",id=name)
               df=rbind(df,temp)
          }
     }

     df
     dflist=list()
     dflist[[1]]=df
     stat=ifelse(mode==1,"Coefficient","OR")
     if(uni){
          fit
          fitlist=fit2list(fit)
          df1=fit2summary(fitlist)
          df1
          names(df1)[2]=paste(stat,"(univariable)")
          df1
          dflist[["uni"]]=df1
     }
     if(multi){
          formula2=paste0(yvar,"~",paste0(result$sigVars,collapse="+"))
          if(mode==1){
               fit2=lm(as.formula(formula2),data=data)
          } else if(mode==2){
               fit2=glm(as.formula(formula2),data=data,family=family)
          }
          df2=fit2summary(fit2)
          names(df2)[2]=paste(stat,"(multivariable)")
          dflist[["multi"]]=df2
          if(imputed & (!final)){
             temp=paste0(ifelse(mode==1,"Coefficients ","OR "),"(imputed)")
             dflist[["imputed"]]= imputedReg(fit2)%>% select(.data$id,.data$stats)
             names(dflist[["imputed"]])[2]=temp
          }
     }
     if(final) {
          formula3=paste0(yvar,"~",paste0(result$finalVars,collapse="+"))
          if(mode==1){
               fit3=lm(as.formula(formula3),data=data)
          } else if(mode==2){
               fit3=glm(as.formula(formula3),data=data,family=family)
          }
          df3=fit2summary(fit3)
          names(df3)[2]=paste(stat,"(final)")

          dflist[["final"]]=df3
          if(imputed){
             temp=paste0(ifelse(mode==1,"Coefficients ","OR "),"(imputed)")
             dflist[["imputed"]]=imputedReg(fit3) %>% select(.data$id,.data$stats)
             names(dflist[["imputed"]])[2]=temp
          }
     }

     Final=reduce(dflist,left_join)
     Final$id=NULL
     names(Final)[1]=paste0("Dependent: ",yvar)
     names(Final)[2]=" "
     Final
     Final %>%  myft()
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
imputedReg=function(fit,data=NULL,m=20,seed=1234,digits=2,...){

     #fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
     #m=20; seed=1234; digits=2
     #xvars = attr(fit$terms, "term.labels")
     xvars=names(fit$model)[-1]
     yvar = as.character(attr(fit$terms, "variables"))[2]

     mode=1
     if("glm" %in% attr(fit,"class")) {
          mode=2
          if(is.null(data)) data=fit$data
     } else{
          if(is.null(data)) data=fit$model
     }

     mydata=data[c(xvars,yvar)]
     mydata
     fmt=paste0("%.",digits,"f")
     formstring=paste0(yvar,"~",paste0(attr(fit$terms, "term.labels"),collapse="+"))

     if(mode==2){
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
                                 sprintf(fmt,.data$upper),",",
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
                                 sprintf(fmt,.data$upper),",",
                                 p2character2(.data$p.value),")")
               ) -> df
     }
     df %>%rename(id=.data$term)
}


#
# fit=glm(status~rx+sex+age+obstruct+nodes,data=colon,family="binomial")
# df=fit2stats(fit) %>%
#      select(id,OR:upper) %>%
#      mutate(type="original")
# imputedReg(fit) %>%
#      select(term,OR:upper) %>%
#      rename(id=term) %>%
#      mutate(type="imputed") %>%
#      bind_rows(df) %>%
#      ggplot(aes(x=OR,y=id,color=type))+
#      geom_point(position=position_dodge(width=0.3))+
#      geom_errorbar(aes(xmin=lower,xmax=upper),
#                    width=0.3,position=position_dodge(width=0.3))+
#      labs(x="Odds Ratio",y="")+
#      theme_bw()
#
# fit=lm(mpg~hp*wt+am,data=mtcars)
#
# df=fit2stats(fit)  %>%
#      select(id,Estimate:upper) %>%
#      mutate(type="original")
# df
# imputedReg(fit,data=mydata) %>%
#      select(term,estimate,lower,upper) %>%
#      rename(id=term,
#             Estimate=estimate) %>%
#      mutate(type="imputed") %>%
#      bind_rows(df) %>%
#      filter(id!="(Intercept)") %>%
#      ggplot(aes(x=Estimate,y=id,color=type))+
#      geom_point(position=position_dodge(width=0.3))+
#      geom_errorbar(aes(xmin=lower,xmax=upper),
#                    width=0.3,position=position_dodge(width=0.3))+
#      labs(x="Estimated coefficients",y="")+
#      theme_bw()
#
# mydata = read_csv("./Rcode/data/anes2008_example.csv") %>%
#      select(FT_mccain,female,ageyr,race3,educ,hhinc,readyBP,economy,environ,iraqwar)
# mydata %>%
#      mutate_at(vars(female,race3,readyBP), ~factor(.)) ->mydata
# mydata
# fit=lm(FT_mccain~female+ageyr+race3+educ+hhinc+readyBP+economy+environ+iraqwar,mydata)
