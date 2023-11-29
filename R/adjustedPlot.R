#' Draw an expected plot
#' @param fit An object of class "coxph" or "survreg"
#' @param xnames Character Names of explanatory variable to plot
#' @param pred.values A list A list of predictor values
#' @param newdata A data.frame or NULL
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param facet Character Name of facet variable
#' @param se logical Whether or not show se
#' @param mark.time logical Whether or not mark time
#' @param show.median logical
#' @param type Character plot type
#' @param ... further arguments to be passed to plot.survfit
#' @return  A ggplot or no return value(called for side effects)
#' @importFrom graphics legend lines
#' @importFrom scales hue_pal
#' @importFrom ggplot2 element_rect facet_wrap label_both ylim facet_grid geom_step
#' @importFrom pammtools geom_stepribbon
#' @export
#' @examples
#' library(survival)
#' fit=coxph(Surv(time,status)~rx+logWBC,data=anderson)
#' adjustedPlot(fit)
#' adjustedPlot(fit,xnames="rx",se=TRUE,type="plot")
#' adjustedPlot(fit,xnames="rx",se=TRUE)
#' \dontrun{
#' anderson$WBCgroup=ifelse(anderson$logWBC<=2.73,0,1)
#' anderson$WBCgroup=factor(anderson$WBCgroup,labels=c("low","high"))
#' anderson$rx=factor(anderson$rx,labels=c("treatment","control"))
#' fit=coxph(Surv(time,status)~rx,data=anderson)
#' adjustedPlot(fit,xnames=c("rx"),show.median=TRUE)
#' fit=coxph(Surv(time,status)~rx*WBCgroup,data=anderson)
#' adjustedPlot(fit,xnames=c("rx","WBCgroup"),show.median=TRUE)
#' adjustedPlot(fit,xnames=c("rx","WBCgroup"),facet="WBCgroup",show.median=TRUE)
#' data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~rx+strata(sex)+age+differ,data =colon)
#'adjustedPlot(fit,xnames=c("sex"))
#'adjustedPlot(fit,xnames=c("sex"),pred.values=list(age=58,differ=3))
#'adjustedPlot(fit,xnames=c("sex","rx"),facet="sex")
#'adjustedPlot(fit,xnames=c("rx","sex","differ"),facet=c("sex","rx"),se=TRUE)
#'fit <- coxph(Surv(start, stop, event) ~ rx + number + size+ cluster(id), data = bladder2)
#'adjustedPlot(fit,xnames=c("rx","number","size"),facet=c("rx","size"),maxy.lev=8)
#'}
adjustedPlot=function(fit,xnames=NULL,pred.values=list(),newdata=NULL,maxy.lev=5,median=TRUE,facet=NULL,se=FALSE,mark.time=FALSE,show.median=FALSE,type="ggplot",...){
     # xnames=c("sex","rx","differ");maxy.lev=5;median=TRUE;facet=c("rx","sex");se=TRUE
     #xnames=c("sex");maxy.lev=5;median=TRUE;facet=NULL;se=TRUE
       #  xnames=c("rx","WBCgroup");facet="WBCgroup";show.median=TRUE
       # pred.values=list();newdata=NULL;maxy.lev=5;median=TRUE;facet=NULL;se=TRUE;mark.time=FALSE;type="ggplot"
     if("survreg" %in% class(fit)) {
          if(type=="ggplot"){
          return(adjustedPlot2.survreg(x=fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev,newdata=newdata,facet=facet,...))
          } else{
               return(adjustedPlot.survreg(x=fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev,...))
          }
     }
     if(is.null(xnames)) {
          return(adjustedPlot2(fit,se=se,mark.time=mark.time))
     }
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xnames)) xnames=xvars[1]

     if(is.null(newdata)){
          newdata=fit2newdata(fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev,median=median)
          labels=attr(newdata,"labels")
     } else{
          labels=attr(newdata,"labels")
          if(is.null(labels)) {
               newdata=addLabelData(newdata)
               labels=attr(newdata,"labels")
          }
     }

     no=length(labels)
     col=scales::hue_pal()(no)

     df1=as_tibble(newdata %>% select(-all_of(xnames)))
     df1=df1[1,]
     label=map2_chr(names(df1),df1,function(x,y){
          paste0(x,"=",y)
     })
     label=paste0(label,collapse=", ")
     if("survfit" %in% class(fit)){
          fit1=fit
     } else{
        fit1=survfit(fit,newdata=newdata)
     }

     if(is.null(facet)){
          if(type=="plot"){
          plot(fit1,col=col,lwd=1,conf.int=se,mark.time=mark.time,...,
               main=paste0("Expected Survival by ",paste0(xnames,collapse=",")))

          legend("bottomleft",legend=labels,col=col,lwd=2)
          title(sub=label)
          }  else{

          #names(fit1$strata)=labels
          #fit1=survfit(fit,newdata=newdata)
          df=survfit2df(fit1,labels=labels)
          df
          p= ggplot(df,aes_string(x="time",y="surv",group="strata",
                                  color="strata"))+
               geom_step()
          if(se==TRUE) {
               p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="strata",color=NULL),alpha=0.3)

          }
          if(mark.time) p<-p+geom_point(data=df[df$n.censor!=0,],shape=3)

          p=p+ theme_classic()+
               theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
               guides(fill="none")+
               ylim(c(0,1))
          p=p+labs(subtitle=label,y="Survival Probability",x="Time")
          if(show.median){
             df2=data.frame(median=summary(fit1)$table[,"median"])
             df2$strata=unique(df$strata)
             df2$x=-Inf
             df2$xend=df2$median
             df2$y=0.5
             for(i in 1:nrow(df2)){
                 p=p+
                   geom_segment(data=df2,aes_string(x="x",xend="xend",y="y",yend="y",color="strata"),lty=2)+
                   geom_segment(data=df2,aes_string(x="xend",xend="xend",y="y",color="strata"),yend=-Inf,lty=2)+
                   geom_text(data=df2,aes_string(x="xend",label="median"),y=-Inf,hjust=-0.5,vjust=-0.5)
             }
          }
          p
          }

     } else{
          if(!is.null(fit1$strata)){
              names(fit1$strata)=labels
              df=survfit2df(fit1)
          } else{
              df=survfit2df(fit1,labels=labels)
          }
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
          myvars=c(xnames,facet)
          for( i in seq_along(myvars)){
              temp=myvars[i]
              if(is.factor(data[[temp]])) {
                 df[[temp]]=factor(df[[temp]],levels=levels(data[[temp]]))
              }
          }
          p= ggplot(df,aes_string(x="time",y="surv",group="newstrata",
                                  color="newstrata"))+
               geom_step()
          if(se==TRUE) {
               p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="newstrata",color=NULL),alpha=0.3)

          }
          if(mark.time) p<-p+geom_point(data=df[df$n.censor!=0,],shape=3)
          p=p+ theme_classic()+
               theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
               guides(fill="none")+
               ylim(c(0,1))

          if(show.median){
            df2=data.frame(median=summary(fit1)$table[,"median"])
            df2$strata=unique(df$strata)
            df2$x=-Inf
            df2$xend=df2$median
            df2$y=0.5
            strata=df2$strata
            suppressMessages(temp<-map_dfc(strata,~strsplit(.,", ")))
            stratalist=list()
            for(i in 1:nrow(temp)){
              x=strsplit(as.character(temp[i,1]),"=")[[1]][1]
              stratalist[[x]]=stringr::str_replace(temp[i,],".*=","")
            }
            df2=cbind(df2,as.data.frame(stratalist))
            df2$newstrata=unique(df$newstrata)
            df2

            for(i in 1:nrow(df2)){
              p=p+
                geom_segment(data=df2,aes_string(x="x",xend="xend",y="y",yend="y",color="newstrata"),lty=2)+
                geom_segment(data=df2,aes_string(x="xend",xend="xend",y="y",color="newstrata"),yend=-Inf,lty=2)+
                geom_text(data=df2,aes_string(x="xend",label="median"),y=-Inf,hjust=-0.5,vjust=-0.5)
            }
          }
          if(length(facet)==1){
            p=p+facet_grid(as.formula(paste0(".~",facet)),labeller=label_both)
          } else {
            myformula=as.formula(paste0(facet[1],"~",facet[2]))
            p=p+facet_grid(myformula,labeller=label_both)
          }
          p=p+labs(subtitle=label,y="Survival Probability",x="Time")
          p
     }
}

#' Draw a survfitted plot
#' @param fit An object of class coxph or survfit
#' @param se logical Whether or not show se
#' @param mark.time logical Whether or not mark time
#' @importFrom pammtools geom_stepribbon
#' @examples
#' library(survival)
#' fit=coxph(Surv(time,status)~rx+logWBC,data=anderson)
#' plot(survfit(fit),conf.int=TRUE)
#' adjustedPlot2(fit,se=TRUE)
#' @return a ggplot
#' @export
adjustedPlot2=function(fit,se=FALSE,mark.time=FALSE){

     if("survfit" %in% class(fit)){
          df=survfit2df(fit)
     } else{
        df=survfit2df(survfit(fit))
     }
     p= ggplot(df,aes_string(x="time",y="surv",group="strata",
                             color="strata"))+
          geom_step()
     if(se==TRUE) {
          p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="strata",color=NULL),alpha=0.3)+
               guides(fill="none")

     }
     if(mark.time) p<-p+geom_point(data=df[df$n.censor!=0,],shape=3)
     p=p+ theme_classic()+
          theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
          labs(y="Survival Probability",x="Time")+
          guides(fill="none")+
          ylim(c(0,1))
     if(length(unique(df$strata))==1) p<-p+guides(color="none",fill="none")
     p
}
