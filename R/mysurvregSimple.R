#'Fit Simple AFT Model
#'
#'Fit Simple AFT Model
#'@param fit An object of class survreg
#'@param threshold	numeric p-value threshold to enter multiple model
#'@param digits integer indicating the position decimal place
#'@param mode integer
#'@return An object of class "data.frame"
#'@examples
#' require(survival)
#' data(cancer)
#' fit=survreg(Surv(time,status)~rx+age+strata(sex)+obstruct+perfor,data=colon)
#' mysurvregSimple(fit)
#'@export
mysurvregSimple=function (fit, threshold = 0.2,digits=2,mode=1)
{
#     threshold = 0.2;digits=2;mode=1
     dataname = as.character(fit$call)[3]
     myt = fit$terms
     temp=as.character(fit$call)[2]
     temp=strsplit(gsub(" ","",temp),"~")
     y=temp[[1]][1]
     myvar = attr(myt, "term.labels")
     myvar
     del=str_detect(myvar,"strata\\(|cluster\\(|frailty\\(")
     if(any(del)) {
          add=myvar[which(del)]
          myvar=myvar[-which(del)]
     } else{
          add=c()
     }

     count = length(myvar)
     var <- HR <- lcl <- ucl <- p <- coef <- se <- z <- c()
     sigVars = c()

     addp=function(x){
          result=c()
          for(i in seq_along(x)){
               if(substr(x[i],1,1)=="<") {
                    temp=paste0("p",x[i])
               } else{
                    temp=paste0("p=",x[i])
               }
               result=c(result,temp)
          }
          result
     }

     result=map_dfr(myvar,function(x){
          temp=paste0(c(x,add),collapse="+")
          s = paste(y, temp, sep = "~")
          temp=paste0("survreg(",s,",data=",dataname,",dist='",fit$dist,"')")
          out <- eval(parse(text = temp))
          result=gaze(out)
          count=nrow(confint(out))
          result=result[2:count,]
          colnames(result)[1]="id"
          result$p1=addp(result$p)
          fmt=paste0("%.",digits,"f")
          if(mode==1){
               result$stats= paste0(sprintf(fmt,as.numeric(result$ETR))," (",
                                    sprintf(fmt,as.numeric(result$LB)),"-",
                                    sprintf(fmt,as.numeric(result$UB)),", ",result$p1,")")
          } else{
               result$stats= paste0(sprintf(fmt,as.numeric(result$HR))," (",
                                    sprintf(fmt,as.numeric(result$lower)),"-",
                                    sprintf(fmt,as.numeric(result$upper)),", ",result$p1,")")
          }
          result$sig=NA
          if(any(summary(out)$table[2:count,4]<threshold)) result$sig=x
          class(result)="data.frame"
          result
     })
     result
     df=result
     sigVars=unique(df$sig)
     attr(df, "sigVars") = sigVars[!is.na(sigVars)]
     df
}
