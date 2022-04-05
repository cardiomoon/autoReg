#' Draw an adjusted Plot for a numeric predictor
#'
#' Select cutpoint for a numeric predictor with maxstat.test() and draw survival plot with this cutpoint
#' @param fit An object of class "coxph"
#' @param xname Character Name of explanatory variable to plot
#' @param no integer Number of groups to be made
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median Logical
#' @param mark.time logical Whether or not mark time
#' @param se logical Whether or not show se
#' @param type Character "plot" or "ggplot"
#' @param ... further arguments to be passed to plot.survfit
#' @return  No return value, called for side effects
#' @importFrom scales hue_pal
#' @importFrom maxstat maxstat.test
#' @importFrom moonBook rank2group
#' @importFrom pammtools geom_stepribbon
#' @export
#' @examples
#' library(survival)
#' data(cancer,package="survival")
#' fit=coxph(Surv(time,status)~age+sex,data =colon)
#' expectedPlot(fit,xname="age")
#' fit=coxph(Surv(time,status)~rx+logWBC,data=anderson)
#' expectedPlot(fit,xname="logWBC",no=3)
expectedPlot=function(fit,xname=NULL,no=2,maxy.lev=5,median=TRUE,mark.time=FALSE,se=FALSE,type="ggplot",...){
     #  xname=c("age");maxy.lev=5;median=TRUE;se=TRUE;no=3;mark.time=TRUE;type="ggplot"
     newdata=fit2newdata(fit,xnames=xname,maxy.lev=maxy.lev,median=median)
     newdata
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     if(is.null(xname)) xname=xvars[1]

     f = fit$formula
     myt = fit$terms
     y = as.character(f)[2]
     data=num2factor(data,call=fit$call,name=xname,no=no)

     call = paste0(y, "~", xname)
     call
     temp=paste0("coxph(",call,",data=data)")
     fit=eval(parse(text=temp))

     labs=levels(data[[xname]])
     newdata1=data.frame(temp=labs)
     names(newdata1)=xname
     if(ncol(newdata)>1){
          for(i in 2:ncol(newdata)){
               newdata1[[colnames(newdata)[i]]]=newdata[1,i]
          }
     }
     newdata1

     labels=labs
     no=length(labels)
     col=scales::hue_pal()(no)
     fit1=survfit(fit,newdata=newdata1)
     df1=as_tibble(newdata1 %>% select(-all_of(xname)))
     df1=df1[1,]
     label=map2_chr(names(df1),df1,function(x,y){
          paste0(x,"=",y)
     })

     label=paste0(label,collapse=", ")
     label

     if(type=="plot"){

          plot(fit1,col=col,lwd=1,conf.int=se,mark.time=mark.time,...,
               main=paste0("Survival Rate by ",paste0(xname,collapse=",")))

          legend("bottomleft",legend=labels,col=col,lwd=2)
          title(sub=label)
     } else{
          df=survfit2df(fit1,labels=labels)

          df$strata=factor(df$strata, levels = labs)
          p= ggplot(df,aes_string(x="time",y="surv",group="strata",
                                  color="strata"))+
               geom_step()
          if(se==TRUE) {
               p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="strata",color=NULL),alpha=0.3)

          }
          if(mark.time) p<-p+geom_point(data=df[df$n.censor!=0,],shape=3)

          p=p+ theme_classic()+
               theme(legend.title=element_blank(),panel.border=element_rect(fill=NA))+
               ylim(c(0,1))
          p=p+labs(subtitle=label,y="Survival Rate",title=paste0("Expected plot by ",xname))
          p
     }
}

#' Convert a numeric column in a data.frame to a factor
#' @param data A data.frame
#' @param call a function call
#' @param name character Name of numeric column
#' @param no numeric
#' @importFrom maxstat maxstat.test
#' @importFrom moonBook rank2group
#' @return A data.frame
#' @export
#' @examples
#' num2factor(anderson,name="logWBC")
#' library(survival)
#' fit=coxph(Surv(time,status)~logWBC+rx,data=anderson)
#' num2factor(anderson,call=fit$call,name="logWBC",no=2)
num2factor=function(data,call,name,no=3){
    if(no==2){
          call=paste0(deparse(call),collapse="")
          temp=c(", data =.*$",".[^\\(]*\\(","^.*=","~.*")
          for(i in seq_along(temp)){
               call=sub(temp[i],"",call)
          }
          call=paste0(call,"~",name)
          mstat = maxstat.test(as.formula(call), data = data, smethod = "LogRank",
                               pmethod = "condMC",B=300)
          cutpoint = mstat$estimate
          data$temp = ifelse(data[[name]] <= cutpoint, 0, 1)

          labs = paste(name,c("\U2264", ">"), cutpoint)
          data$temp = factor(data$temp, levels = c(0, 1), labels = labs)
     } else{
          data$temp=rank2group(data[[name]],no)
          if(no==3) {
               labs=c("Low","Medum","High")
          } else{
               labs=1:no
          }
          data$temp = factor(data$temp, levels = 1:no, labels = labs)

     }
     data[[name]]=data$temp
     data$temp=NULL
     data
}
