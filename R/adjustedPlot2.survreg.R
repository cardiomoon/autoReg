#'Draw predicted survival curve as a ggplot with an object survreg
#'
#' @param x An object of class survreg
#' @param xnames Character Names of explanatory variable to plot
#' @param pred.values A list A list of predictor values
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param newdata A data.frame or NULL
#' @param addCox  logical Whether or not add KM
#' @param autovar logical
#' @param legend.position Character Default value is "topright"
#' @param facet Character name(s) of facet variable(s)
#' @importFrom ggplot2 scale_linetype_manual guide_legend xlim
#' @export
#' @return A ggplot
#' @examples
#' library(survival)
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' adjustedPlot(x,type="plot")
#' adjustedPlot(x)
#' adjustedPlot(x,addCox=TRUE)
#' \dontrun{
#' x=survreg(Surv(time, status) ~ sex, data=lung,dist="weibull")
#' adjustedPlot(x,addCox=TRUE)
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' adjustedPlot(x,addCox=TRUE)
#' x=survreg(Surv(time, status) ~ ph.ecog + age + sex, data=lung, dist="weibull")
#' pred.values=list(ph.ecog=0:3,sex=1:2,age=c(20,40,60,80))
#' adjustedPlot(x)
#' adjustedPlot(x,addCox=TRUE)
#' adjustedPlot(x,addCox=TRUE,xnames=c("ph.ecog","sex"),facet="sex")
#' adjustedPlot(x,pred.values=pred.values,addCox=TRUE,legend.position="top")+xlim(c(1,1000))
#' adjustedPlot(x,pred.values=pred.values,xnames=c("ph.ecog","sex","age"),facet=c("ph.ecog","sex"))
#' adjustedPlot(x,pred.values=pred.values,xnames=c("ph.ecog","sex","age"),facet=c("age","sex"))
#' adjustedPlot(x,pred.values=pred.values,addCox=TRUE)
#' adjustedPlot(x,addCox=TRUE)
#' adjustedPlot(x,pred.values=list(age=c(20,40,60,80),sex=1,ph.ecog=3),addCox=TRUE)
#' }
adjustedPlot2.survreg=function(x,xnames=NULL,pred.values=list(),maxy.lev=5,
                              newdata=NULL,addCox=FALSE,autovar=TRUE,
                              legend.position=NULL,
                              facet=NULL){
        # x=survreg(Surv(time, status) ~ ph.ecog + age + sex, data=lung, dist="weibull")
        # xnames=c("ph.ecog","sex");facet="sex";legend.position=NULL;pred.values=list();newdata=NULL
        # maxy.lev=5;addCox=FALSE;autovar=TRUE
       # pred.values=list();newdata=NULL;facet=NULL;
       #pred.values=list(ph.ecog=0:3,sex=1:2,age=c(20,40,60,80))
       #newdata=NULL;
       #addCox=TRUE;
      #pred.values=list(age=c(20,40,60,80),sex=2,ph.ecog=3)
     fit=x
     xvars = attr(fit$terms, "term.labels")
     if(length(xvars)>0){

          if(is.null(newdata)){
               newdata=fit2newdata(fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev)
               labels=attr(newdata,"labels")
          } else{
               labels=attr(newdata,"labels")
               if(is.null(labels)) {
                    newdata=addLabelData(newdata)
                    labels=attr(newdata,"labels")
               }
          }
          if(autovar){
                    temp=names(newdata)
                    xvars=c()
                    for(i in seq_along(temp)){
                           if(length(unique(newdata[[temp[i]]]))>1) {
                                xvars=c(xvars,temp[i])
                           }
                    }
                    xvars
                    if(is.null(xnames)) xnames=xvars[1]
                    if(is.null(facet)) facet=setdiff(xvars,xnames)
          }
          newdata
          temp=map2(newdata,names(newdata),function(x,y){
               ifelse(length(unique(x))==1,y,"")
          })
          temp=unlist(setdiff(temp,""))
          df1=as_tibble(newdata %>% select(all_of(temp)))
          df1=df1[1,]
          label=map2_chr(names(df1),df1,function(x,y){
               paste0(x,"=",y)
          })
          label=paste0(label,collapse=", ")
          no=length(labels)

     } else{
          no=1
     }
     newdata
     xvars
     xnames
     facet
     if(addCox){
          fitcox=coxph(as.formula(deparse(fit$terms)),data=fit2model(fit))
          if(length(xvars)>0){
               fit1=survfit(fitcox,newdata=newdata)
          }  else {
               labels=""
               fit1=survfit(fitcox)
          }
          coxdf=survfit2df(fit1,labels=labels)
          coxdf

     }


     p = seq(0.01, 0.99, by=.01)

     if(length(xvars)>0){
          ptime <- predict(fit, newdata=newdata, type='quantile',
                           p = p, se=TRUE)

     } else{
          ptime <- predict(fit,  type='quantile',
                           p =p, se=TRUE)
          ptime$fit=ptime$fit[1,]
          ptime$se.fit=ptime$se.fit[1,]
          label=""
     }

     if("matrix" %in% class(ptime$fit)){
          px=as_tibble(t(ptime$fit),.name_repair="minimal")
     } else{
          px=as_tibble(ptime$fit,.name_repair="minimal")
     }

     names(px)=labels
     px
     surv=1-p
     df1=cbind(surv,px)
     df1
     longdf=pivot_longer(df1,cols=-surv,names_to="strata",values_to="time")

     if(addCox){
         longdf$model="1"
         df2=coxdf[,c("surv","strata","time")]
          df2$model="2"
          finaldf=rbind(longdf,df2)
     } else{
          longdf$model="1"
          finaldf=longdf
     }
     finaldf=cbind(finaldf,strata2df(finaldf$strata))
     finaldf
     xnames
     table(finaldf$model)
     xvar=setdiff(xnames,facet)[1]
     p=ggplot(finaldf,aes_string(x="time",y="surv",color=xvar))
     if(addCox) {
         p=p+geom_line(data=finaldf[finaldf$model!="Cox",],aes_string(linetype="model"))+
             geom_step(data=finaldf[finaldf$model=="Cox",],aes_string(linetype="model"))
         p=p+scale_linetype_manual(values=c(1,2),labels=c(fit$dist,"Cox"))
     } else{
          p=p+geom_line()
     }
     p

     p=p+theme_classic()+
          theme(panel.border=element_rect(fill=NA))
     if(length(facet)>0){
          if(length(facet)==1){
               p=p+facet_grid(as.formula(paste0(".~",facet)),labeller=label_both)
          } else {
               myformula=as.formula(paste0(facet[1],"~",paste0(facet[-1],collapse="+")))
               p=p+facet_grid(myformula,labeller=label_both)
          }
     }
     if(is.null(legend.position)){

     if(length(facet)>0){
          legend.position="top"
     } else{
          legend.position=c(0.8,0.8)
     }
     }
     p=p+theme(legend.position=legend.position)+guides(color=guide_legend(title=xvar))
     p =p+labs(subtitle=label,y="Survival Probability",x="Time")
     if(addCox) p=p+xlim(c(0,max(coxdf$time)))

     p

}
