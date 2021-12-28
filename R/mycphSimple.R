#'Fit Simple Proportional Hazards Regression Model
#'
#'Fit Simple Proportional Hazards Regression Model
#'@param fit An object of class coxph
#'@param threshold	numeric p-value threshold to enter multiple model
#'@param digits interger indicating the position decimal place
#'@examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#'@export
mycphSimple=function (fit, threshold = 0.2,digits=2)
{
     dataname = as.character(fit$call)[3]
     f = fit$formula
     myt = fit$terms
     as.character(f)
     y = as.character(f)[2]
     myvar = attr(myt, "term.labels")
     myvar
     count = length(myvar)
     var <- HR <- lcl <- ucl <- p <- coef <- se <- z <- c()
     sigVars = c()
     count
     for (i in 1:count) {
          s = paste(y, myvar[i], sep = "~")
          temp = paste0("summary(coxph(", s, ",data=", dataname,
                        "))")
          out <- eval(parse(text = temp))
          if (any(is.infinite(out$conf.int))) {
               cat(dimnames(out$conf.int)[[1]], " was excluded : infinite\n")
               next
          }
          if (any(is.nan(out$coef))) {
               cat(dimnames(out$conf.int)[[1]], " was excluded : NaN\n")
               next
          }
          coef = c(coef, out$coef[, 1])
          HR = c(HR, out$coef[, 2])
          lcl = c(lcl, out$conf.int[, 3])
          ucl = c(ucl, out$conf.int[, 4])
          se <- c(se, out$coef[, 3])
          z = c(z, out$coef[, 4])
          p = c(p, out$coef[, 5])
          var = c(var, attr(out$coef, "dimnames")[[1]])
          if (any(out$coef[, 5] <= threshold))
               sigVars = c(sigVars, myvar[i])
     }
     if (length(HR) < 1)
          return(invisible())
     result = data.frame(coef, HR, lcl, ucl, se, z, p)
     rownames(result) = var
     attr(result, "sigVars") = sigVars
     result
     names(result)[3:4]=c("lower","upper")
     result$id=rownames(result)
     fmt=paste0("%.",digits,"f")
     result$stats=paste0(sprintf(fmt,result$HR)," (",
                         sprintf(fmt,result$lower),"-",
                         sprintf(fmt,result$upper),", ",p2character2(result$p),")")
     df=result
     df
}


#' Make multivariate regression model by selecting univariate models with p.value below threshold
#' @param fit An abject of class coxph
#' @param threshold Numeric
#' @examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' fit2multi(fit)
#' @export
fit2multi=function(fit,threshold=0.2){
     if(threshold>=1){
          fit
     } else{
          uni=mycphSimple(fit,threshold=threshold)
          xvars=attr(uni,"sigVars")
          if(length(xvars>0)){
               temp = as.character(fit$call)
               dataname = as.character(fit$call)[3]
               f = fit$formula
               y = as.character(f)[2]
               temp4 = paste0(temp[1], "(", y, "~", paste0(xvars, collapse = "+"),
                              ",data=",dataname,")")
               multiModel=eval(parse(text=temp4))
          }
          fit=multiModel
     }
     fit
}


#' Make final model using stepwise backward elimination
#' @param fit An object of class coxph
#' @param threshold Numeric
#' @importFrom survival coxph Surv
#' @importFrom stats na.omit
#' @examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' final=fit2final(fit)
#' fit2summary(final)
#' @export
fit2final=function(fit,threshold=0.2){

     fit=fit2multi(fit,threshold=threshold)
     temp = as.character(fit$call)
     f = fit$formula
     y = as.character(f)[2]

     timevar=attr(fit$y,"dimnames")[[2]][1]
     statusvar=attr(fit$y,"dimnames")[[2]][2]
     dataname = as.character(fit$call)[3]
     xvars = attr(fit$term, "term.labels")
     xvars2 = c(xvars, timevar, statusvar)
     temp3 = paste0(dataname, "[", paste0("c('",paste0(xvars2,collapse="','"),"')"), "]")
     temp3
     data1 = eval(parse(text = temp3))
     data1 = na.omit(data1)
     temp4 = paste0(temp[1], "(", y, "~", paste0(xvars, collapse = "+"),
                    ",data=data1)")
     temp4
     multiple2 = eval(parse(text = temp4))
     final = step(multiple2, direction = "backward", trace = 0)
     final
}


#'@describeIn autoReg S3 method for a class coxph
#'@export
autoReg.coxph=function(x,...){
     autoRegCox(x,...)
}

#' perform automatc regression for a class of coxph
#'@param x An object of class coxph
#'@param data A data.frame
#'@param threshold numeric
#'@param uni logical whether or not perform univariate regression
#'@param multi logical whether or not perform multivariate regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param imputed logical whether or not perform multiple imputation
#'@param keepid logical whether or not save id column
#'@param keepstats logical whether or not keep statistic
#'@examples
#' require(survival)
#' require(magrittr)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' autoReg(fit,data=colon)
#' autoReg(fit,data=colon,uni=TRUE,threshold=1)
#' autoReg(fit,data=colon,uni=TRUE,final=TRUE) %>% myft()
#' data(colon_s,package="finalfit")
#' fit=coxph(Surv(time,status)~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s)
#' autoReg(fit,data=colon_s,uni=TRUE,threshold=1)
#' autoReg(fit,data=colon_s,uni=TRUE,imputed=TRUE)
#'@export
autoRegCox=function(x,data,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,imputed=FALSE,keepid=FALSE,keepstats=FALSE){
         # x=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
         # data=colon_s;threshold=0.2;uni=FALSE;multi=TRUE;final=FALSE;imputed=TRUE;keepid=TRUE;keepstats=TRUE
     if(uni==FALSE) threshold=1
     fit=x
     dataname = as.character(fit$call)[3]
     if(missing(data)) {
          data=eval(parse(text=dataname))
     }
     timevar=attr(fit$y,"dimnames")[[2]][1]
     statusvar=attr(fit$y,"dimnames")[[2]][2]
     xvars = attr(fit$terms, "term.labels")
     myformula=paste0("~",paste0(xvars,collapse="+"))
     mylist=list()
     mylist[[1]]=gaze(as.formula(myformula),data=data,keepid=TRUE)
     names(mylist[[1]])[1:3]=c(paste0("Dependent: Surv(",timevar,",",statusvar,")")," ","all")
     names(mylist[[1]])[1:3]
     no=2
     if(uni){
          df=mycphSimple(fit,threshold=threshold)
          if(keepstats){
               df=df[c(2:4,7:9)]
               df$mode="univariate"
          } else{
               df=df[c(8:9)] %>%
                    rename("HR (univariate)"=.data$stats)
          }
          mylist[[no]]=df
          no=no+1
     }
     if(multi){
          fit=fit2multi(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(fit)
               df$mode="multivariate"
          } else{
               df=fit2summary(fit) %>%
                    rename("HR (multivariate)"=.data$stats)
          }
          mylist[[no]]=df
          no=no+1
     }
     if(final){
          final=fit2final(fit,threshold=threshold)
          if(keepstats){
               df=fit2stats(fit)
               df$mode="final"
          } else{
               df=fit2summary(fit) %>%
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
          names(Final)[1]=paste0("Dependent: Suv(",timevar,",",statusvar,")")
          names(Final)[2]=" "
          if(!keepid) Final$id=NULL
          Final
     }
     class(Final)=c("autoReg","data.frame")
     Final[is.na(Final)]=""
     Final

}


#' Add model summary to an object of class gaze
#' @param df An object of class gaze
#' @param fit An object of class glm or lm
#' @param statsname character Name of statistics
#' @param keepid logical whether or not save id
#' @export
#' @examples
#' require(survival)
#' require(magrittr)
#' data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~rx+age+sex+nodes+obstruct+perfor,data=colon)
#' df=autoReg(fit,data=colon,uni=FALSE,keepid=TRUE)
#' final=fit2final(fit)
#' df %>% addFitSummary(final,statsname="HR (final)",keepid=FALSE) %>% myft()
addFitSummary=function(df,fit,statsname="",keepid=FALSE){
     result=fit2summary(fit)
     if(statsname!="") {
          names(result)[names(result)=="stats"]= statsname
     }
     df <-df %>% left_join(result)
     if(!keepid) df$id=NULL
     df
}

