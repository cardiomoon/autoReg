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


#' Make final model using stepwise backward elimination
#' @param fit An abject of class coxph
#' @importFrom survival coxph Surv
#' @importFrom stats na.omit
#' @examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' final=fit2final(fit)
#' fit2summary(fit)
#' @export
fit2final=function(fit){
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
     multiple2
     final = step(multiple2, direction = "backward", trace = 0)
     final
}

#'Perform univariable and multivariable regression and stepwise backward regression automatically
#'@param fit An object of class coxph
#'@param data A data.frame
#'@param threshold numeric
#'@param uni logical whether or not perform univariate regression
#'@param multi logical whether or not perform multivariate regression
#'@param final logical whether or not perform stepwise backward elimination
#'@param saveid logical whether or not save id column
#'@examples
#' require(survival)
#' require(magrittr)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' autoRegCox(fit,data=colon) %>% myft()
#' autoRegCox(fit,data=colon,uni=TRUE,threshold=1) %>% myft()
#' autoRegCox(fit,data=colon,uni=TRUE,final=TRUE) %>% myft()
#' data(colon_s,package="finalfit")
#' fit=coxph(Surv(time,status)~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s)
#' autoRegCox(fit,data=colon_s,uni=TRUE,threshold=1) %>% myft()
#' autoRegCox(fit,data=colon_s,uni=TRUE,threshold=0.2) %>% myft()
#'@export
autoRegCox=function(fit,data,threshold=0.2,uni=FALSE,multi=TRUE,final=FALSE,saveid=FALSE){
        # data=colon_s;threshold=0.2;uni=TRUE;multi=TRUE;final=FALSE;saveid=FALSE
     timevar=attr(fit$y,"dimnames")[[2]][1]
     statusvar=attr(fit$y,"dimnames")[[2]][2]
     xvars = attr(fit$terms, "term.labels")
     myformula=paste0("~",paste0(xvars,collapse="+"))
     mylist=list()
     mylist[[1]]=mySummary(as.formula(myformula),data=data,keepid=TRUE)
     names(mylist[[1]])[1:3]=c(paste0("Dependent: Surv(",timevar,",",statusvar,")")," ","all")
     names(mylist[[1]])[1:3]
     no=2
     if(uni){
          mylist[[no]]=mycphSimple(fit,threshold=threshold) %>%
               select(.data$id,.data$stats) %>%
               rename("HR (univariate)"=.data$stats)
          no=no+1
          if(threshold<1){
               xvars=attr(mylist[[2]],"sigVars")
               if(length(xvars>0)){
               temp = as.character(fit$call)
               dataname = as.character(fit$call)[3]
               f = fit$formula
               y = as.character(f)[2]
               temp4 = paste0(temp[1], "(", y, "~", paste0(xvars, collapse = "+"),
                              ",data=",dataname,")")
               multiModel=eval(parse(text=temp4))
               fit=multiModel
               }
          }
     }
     if(multi){
          mylist[[no]]=fit2summary(fit) %>%
               select(.data$id,.data$stats) %>%
               rename("HR (multivariate)"=.data$stats)
          no=no+1
     }
     if(final){
          final=fit2final(fit)
          mylist[[no]]=fit2final(fit) %>%
               fit2summary() %>%
               select(.data$id,.data$stats) %>% rename("HR (final)"=.data$stats)
     }
     result=reduce(mylist,left_join)
     if(!saveid) result$id=NULL
     result
}

#'Extract Harzard Ratio
#'
#'Extract Harzard Ratio
#'@param x An object of class coxph
#'@param digits integer indicating the number of decimal places
#'@examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' extractHR2(fit)
#'@export
extractHR2=function (x, digits = 4)
{
     out = summary(x)
     a = out$conf.int
     b = out$coef
     res = data.frame(a[, 1], a[, 3], a[, 4])
     res = round(res, 2)
     res = cbind(res, round(b[, 5], max(3, digits)))
     colnames(res) = c("HR", "lcl", "ucl", "p")
     rownames(res) = rownames(a)
     res
}

#' Add model summary to an object of class mySummary
#' @param df AN object of class mySummary
#' @param fit An object of class glm or lm
#' @param statsname character Name of statistics
#' @param saveid logical whether or not save id
#' @export
#' @examples
#' require(lme4)
#' data(colon_s,package="finalfit")
#' fit=glm(mort_5yr~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s,family="binomial")
#' df=autoReg(fit,uni=TRUE,multi=TRUE,threshold=1,saveid=TRUE)
#' fit2=lme4::glmer(mort_5yr~age.factor+obstruct.factor+(1|hospital),data=colon_s,family="binomial")
#' df %>% addFitSummary(fit2,statsname="OR (multilevel)") %>% myft()
addFitSummary=function(df,fit,statsname="",saveid=FALSE){
     result=fit2summary(fit)
     if(statsname!="") {
          names(result)[names(result)=="stats"]= statsname
     }
     df <-df %>% left_join(result)
     if(!saveid) df$id=NULL
     df
}

