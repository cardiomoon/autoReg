#' Draw an expected plot
#' @param fit An object of class "coxph"
#' @param xnames Character Names of explanatory variable to plot
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param facet Character Name of facet variable
#' @param se logical Whether or not show se
#' @param type Character plot type
#' @param ... further arguments to be passed to plot.survfit
#' @return  No return value, called for side effects
#' @importFrom graphics legend lines
#' @importFrom scales hue_pal
#' @importFrom ggplot2 element_rect facet_wrap label_both ylim facet_grid
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~rx+strata(sex)+age+differ,data =colon)
#'expectedPlot(fit,xnames=c("sex"))
#'expectedPlot(fit,xnames=c("rx","sex","differ"),facet=c("sex","rx"))
expectedPlot=function(fit,xnames=NULL,maxy.lev=5,median=TRUE,facet=NULL,se=FALSE,type="ggplot",...){
     # xnames=c("sex","rx","differ");maxy.lev=5;median=TRUE;facet=c("rx","sex");se=TRUE
     #xnames=c("sex");maxy.lev=5;median=TRUE;facet=NULL;se=TRUE
     newdata=fit2newdata(fit,xnames=xnames,maxy.lev=maxy.lev,median=median)
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xnames)) xnames=xvars[1]
     labels=attr(newdata,"labels")
     labels
     no=length(labels)
     col=scales::hue_pal()(no)
     fit1=survfit(fit,newdata=newdata)
     df1=as_tibble(newdata %>% select(-all_of(xnames)))
     df1=df1[1,]
     label=map2_chr(names(df1),df1,function(x,y){
          paste0(x,"=",y)
     })
     label=paste0(label,collapse=", ")

     if(is.null(facet)){
          if(type=="plot"){
          plot(fit1,col=col,lwd=1,conf.int=se,...,
               main=paste0("Expected Survival by ",paste0(xnames,collapse=",")))
          legend("bottomleft",legend=labels,col=col,lwd=2)
          title(sub=label)
          }  else{
          names(fit1$strata)=labels
          df=survfit2df(fit1)
          df
          p= ggplot(df,aes_string(x="time",y="surv",group="strata",
                                  color="strata"))+
               geom_line()
          if(se==TRUE) {
               p=p+geom_ribbon(aes_string(ymin="lower",ymax="upper",fill="strata",color=NULL),alpha=0.3)

          }
          p=p+ theme_classic()+
               theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
               ylim(c(0,1))
          p=p+labs(subtitle=label,y="Expected Survival")
          p+theme(legend.position=c(0.85,0.9))
          }

     } else{
          names(fit1$strata)=labels
          df=survfit2df(fit1)
          newvar=setdiff(xnames,facet)

          suppressMessages(res<-map_dfc(newvar,function(x){
               paste0(x,"=",df[[x]])
          }))
          res
          names(res)=newvar
          newvar
          if(length(newvar)>0){
               df$newstrata=apply(res,1,paste,collapse=", ")
          } else{
               df$newstrata=1
          }
          df

          p= ggplot(df,aes_string(x="time",y="surv",group="newstrata",
                                  color="newstrata"))+
               geom_line()
          if(se==TRUE) {
               p=p+geom_ribbon(aes_string(ymin="lower",ymax="upper",fill="newstrata",color=NULL),alpha=0.3)

          }
          p=p+ theme_classic()+
               theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
               ylim(c(0,1))
          if(length(facet)==1){
               p=p+facet_wrap(as.formula(paste0("~",facet)),labeller=label_both)
          } else {
               myformula=as.formula(paste0(facet[1],"~",facet[2]))
               p=p+facet_grid(myformula,labeller=label_both)
          }
          p=p+labs(subtitle=label)
          p
     }
}
