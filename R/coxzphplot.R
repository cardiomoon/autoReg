#' Graphical Test of Proportional Hazards
#'
#' Tis is a ggplot version of plot.cox.zph.
#' Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve.
#' @param x result of the cox.zph function.
#' @param resid a logical value, if TRUE the residuals are included on the plot, as well as the smooth fit.
#' @param se	a logical value, if TRUE, confidence bands at two standard errors will be added.
#' @param var The set of variables for which plots are desired. It can be integer or variable names
#' @param hr logical If true, plot for hazard ratio, If false, plot for coefficients
#' @param add.lm logical If true, add lenear regression line
#' @return A facetted ggplot
#' @importFrom tidyr pivot_longer
#' @importFrom stats approx
#' @importFrom ggplot2 scale_x_continuous scale_x_log10 scale_y_log10 as_labeller
#' @export
#' @examples
#' library(survival)
#' vfit <- coxph(Surv(time,status) ~ trt + factor(celltype) + karno + age, data=veteran, x=TRUE)
#' x <- cox.zph(vfit)
#' coxzphplot(x)
#' coxzphplot(x,var="karno",add.lm=TRUE)
coxzphplot=function(x,resid=TRUE,se=TRUE,var=NULL,hr=FALSE,add.lm=FALSE){
     #resid=TRUE;se=TRUE;var="karno";hr=FALSE;add.lm=TRUE
     if(is.null(var)){
          var=1:ncol(x$y)
     } else if (is.character(var)) {
          var <- match(var, dimnames(x$y)[[2]])
     }
     varnames=dimnames(x$y)[[2]][var]

     df=as.data.frame(x$y)[var]
     df=cbind(df,data.frame(x=x$x))

     longdf=df %>% pivot_longer(cols=all_of(varnames))

     if (x$transform == "log") {
          longdf$x <- exp(longdf$x)
     } else if (x$transform != "identity") {
          xx=x$x
          xtime <- x$time
          indx <- !duplicated(xx)
          apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx),
                                                    length = 17)[2 * (1:8)])
          temp <- signif(apr1$y, 2)
          apr2 <- approx(xtime[indx], xx[indx], temp)
          xaxisval <- apr2$y
          xaxislab <- rep("", 8)
          for (i in 1:8) xaxislab[i] <- format(temp[i])
     }

     if (hr) {
          ylab <-"HR(t)"
     } else {
          ylab <- "Beta(t)"
     }
     if(hr) longdf$value=exp(longdf$value)

     p=ggplot(longdf,aes_string(x="x",y="value"))
     if(resid) p=p+geom_point(alpha=0.5)
     if(se) p=p+stat_smooth(method="loess",formula="y~x")
     if(add.lm) p=p+stat_smooth(method="lm",formula="y~x",color="red",se=FALSE)

     pvalue=x$table[nrow(x$table),ncol(x$table)]

     dfp=data.frame(p=paste0("Schoenfeld Test ",p2character2(x$table[var,3],add.p=TRUE)),
                    name=varnames)
     mylabeller=paste(ylab,"for",varnames)
     names(mylabeller)=varnames

     p=p+  theme_classic()+
          labs(x="Time",y=ylab,title=paste0("Global test for PH assumption : ",p2character2(pvalue,add.p=TRUE)))

     if(x$transform=="log") {
          p=p+scale_x_log10()
     } else if(x$transform!="identity"){
          p=p+ scale_x_continuous(breaks=xaxisval,labels=xaxislab)
     }
     if(hr) p=p+scale_y_log10()
     p=p+facet_wrap(~name,scales="free_y")
     # p=p+facet_wrap(~name,scales="free_y",strip.position="left",
     #                labeller=as_labeller(mylabeller))+
     #      theme(strip.background = element_blank(),
     #            strip.placement = "outside")
     p=p+geom_text(data=dfp,aes(x=-Inf,y=Inf,label=p),hjust=-0.1,vjust=2)+
          theme(plot.title=element_text(hjust=0.5))
     p

}
