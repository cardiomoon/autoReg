#' Draw a martingale residual plot with an object of class coxph
#' @param fit An object of class coxph
#' @param mode Integer If mode is 1, draw martingale residual plot.
#' If mode is 2, draw dfbeta plot. if mode is 3, draw risk score plot
#' @importFrom ggplot2 facet_wrap stat_smooth
#' @importFrom tidyr pivot_longer
#' @importFrom stats residuals
#' @examples
#' require(survival)
#' fit=coxph(Surv(time,status==2)~log(bili)+age+cluster(edema),data=pbc)
#' residualPlot(fit)
#' @export
#' @return A ggplot
residualPlot=function(fit,mode=1){
     data=fit2model(fit)
     fit
     f = fit$formula
     y = as.character(f)[2]
     y
     temp1=str_remove_all(y,"Surv\\(|\\)| ")
     temp1=unlist(strsplit(temp1,","))
     timevar=temp1[1]
     statusvar=temp1[2]
     xvars = attr(fit$terms, "term.labels")
     xvars
     del=str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")
     if(any(del)) xvars=xvars[-which(del)]

     no=length(xvars)
     # data=data[xvars]
     myformula=paste0("Surv(",timevar,",",statusvar,")~1")
     fit0=coxph(as.formula(myformula),data=data)
     r1=resid(fit0,res="martingale")
     data$r1=r1
     longdf=data %>% pivot_longer(cols=all_of(xvars),
                                  names_to="xvars")
     if(mode==1){
          p=ggplot(longdf,aes_string(x="value",y="r1"))+
               geom_point(alpha=0.3)+
               stat_smooth(method="loess",formula="y~x")+
               facet_wrap(~xvars,scales="free")+
               labs(x="",y="martingale residuals")+
               theme_bw()
     } else{

          dfbeta=residuals(fit,type="dfbeta")
          colnames(dfbeta)=xvars
          dfbeta=as.data.frame(dfbeta)
          dfbeta$index=1:nrow(dfbeta)

          for(i in seq_along(xvars)){
               if(i==1) {
                    riskscore=fit$coef[i]*data[[xvars[i]]]
               } else{
                    riskscore=fit$coef[i]*data[[xvars[i]]]
               }
          }

          dfbeta$riskscore=riskscore
          dfbeta %>% pivot_longer(cols=all_of(xvars)) -> longdf

          if(mode==2) {
               p= ggplot(longdf,aes_string(x="index",y="value"))+
                    geom_point(alpha=0.3)+
                    facet_wrap(~name,scales="free")+
                    labs(x="index",y="dfbeta")+
                    theme_bw()
          } else{
               p= ggplot(longdf,aes_string(x="riskscore",y="value"))+
                    geom_point(alpha=0.3)+
                    facet_wrap(~name,scales="free")+
                    labs(x="risk score",y="")+
                    theme_bw()
          }
     }
     p

}
