#'Draw predicted survival curve with an object survreg
#'
#' @param x An object of class survreg
#' @param xnames Character Names of explanatory variable to plot
#' @param pred.values A list A list of predictor values
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param newdata A data.frame or NULL
#' @param fillalpha Numeric alpha value for geom_ribbon
#' @param se logical Whether or not show se
#' @param legend.position legend position
#' @return A ggplot
#' @examples
#' library(survival)
#' x=survreg(Surv(time, status) ~ rx, data=anderson,dist="exponential")
#' adjustedPlot(x)
#' x=survreg(Surv(time, status) ~ ph.ecog + age + strata(sex), lung)
#' adjustedPlot(x)
#' @export
adjustedPlot.survreg=function(x,xnames=NULL,pred.values=list(),maxy.lev=5,median=TRUE,newdata=NULL,fillalpha=0.3,se=FALSE,legend.position=c(0.8,0.9)){
     # xnames=NULL;pred.values=list();maxy.lev=5;median=TRUE;
     # newdata=NULL;fillalpha=0.3;se=FALSE;legend.position=c(0.8,0.9)
     fit=x
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xnames)) xnames=xvars[1]
     if(is.null(newdata)){
          newdata=fit2newdata(fit,xnames=xnames,pred.values=pred.values,maxy.lev=maxy.lev,median=median)
          labels=attr(newdata,"labels")
     }
     df1=as_tibble(newdata %>% select(-all_of(xnames)))
     df1=df1[1,]
     label=map2_chr(names(df1),df1,function(x,y){
          paste0(x,"=",y)
     })
     label=paste0(label,collapse=", ")

     pct <- 1:98/100   # The 100th percentile of predicted survival is at +infinity
     ptime <- predict(fit, newdata=newdata, type='quantile',
                      p=pct, se=TRUE)
     x=as_tibble(t(ptime$fit),.name_repair="minimal")
     names(x)=paste0("group",1:ncol(x))
     y=1-pct
     se1=as_tibble(t(ptime$se.fit),.name_repair="minimal")
     names(se1)=paste0("group",1:ncol(se1))
     df1=cbind(y,x)
     longdf1=pivot_longer(df1,cols=-.data$y,names_to="group",values_to="x")
     df2=cbind(y,se1)
     longdf2=pivot_longer(df2,cols=-.data$y,names_to="group",values_to="se")
     longdf=left_join(longdf1,longdf2,by=c("y","group"))
     longdf$xmax=longdf$x+2*longdf$se
     longdf$xmin=longdf$x-2*longdf$se
     longdf$group=factor(longdf$group)
     if(!is.null(labels)) levels(longdf$group)=labels
     p=ggplot(longdf,aes_string(x="x",y="y"))+geom_line(aes_string(color="group"))
     if(se) p=p+ geom_ribbon(aes_string(xmin="xmin",xmax="xmax",fill="group"),alpha=fillalpha)
     p=p+ theme_classic()+
          theme(legend.title=element_blank(),
                panel.border=element_rect(fill=NA),
                legend.position=legend.position)+
          labs(y="Survival Probability",x="time",subtitle=label)
     p
}
