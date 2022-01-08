#'Fit Simple Proportional Hazards Regression Model
#'
#'Fit Simple Proportional Hazards Regression Model
#'@param fit An object of class coxph
#'@param threshold	numeric p-value threshold to enter multiple model
#'@param digits integer indicating the position decimal place
#'@return An object of class "data.frame"
#'@examples
#' require(survival)
#' data(cancer)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' mycphSimple(fit)
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
     del=str_detect(myvar,"strata\\(|cluster\\(|frailty\\(")
     if(any(del)) myvar=myvar[-which(del)]

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



