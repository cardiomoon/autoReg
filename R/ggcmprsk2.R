#'Compare cumulative incidence to th Kaplan-Meier estimate
#'@param x A formula as time+status~1
#'@param data A data.frame
#'@param id Character vector of length2
#'@param se logical whether or not show confidence interval
#'@param xpos numeric x-axis position of label
#'@param ypos numeric y-axis position of label
#'@param ylabs string vector of length 2. y axis labels
#'@param xlab A character. The x-axis label
#'@param label string vector of length 2. Label names
#'@param plot logical Whether or not print plot
#'@importFrom tidycmprsk cuminc
#'@importFrom survival survfit Surv
#'@importFrom ggplot2 scale_y_continuous sec_axis
#'@return A list containing the following components:
#'\describe{
#'   \item{df}{A long-form data.frame consist of time, est, upper,lower, id, method}
#'   \item{df3}{A data.frame for label consist of x, y, label, id}
#'   \item{p}{A ggplot object}
#'}
#'@export
#'@examples
#'require(dplyr)
#'data(prostateSurvival,package="asaur")
#'prostateHighRisk <- prostateSurvival %>%
#'   filter(grade=="poor" & stage=="T2",ageGroup=="80+")
#'ggcmprsk2(survTime/12+status~1,data=prostateHighRisk,
#'   id=c("prostate cancer","other causes"))
ggcmprsk2=function(x,data,id=c("disease","other"),se=FALSE,
                    xpos=c(2,2),ypos=c(0.25,0.70),
                    ylabs=NULL,xlab=NULL,label=NULL,plot=TRUE){
     # data(prostateSurvival,package="asaur")
     # prostateHighRisk <- prostateSurvival %>%filter(grade=="poor" & stage=="T2",ageGroup=="80+")
     # x=survTime/12+status~1;data=prostateHighRisk;id=c("prostate cancer","other causes")
     # se=FALSE;xpos=c(2,2);ypos=c(0.25,0.70)
     # ylabs=NULL;xlab=NULL;label=NULL;plot=TRUE

        temp=strsplit(deparse(x),"~")[[1]][1]
        temp=gsub(" ","",temp)
        yvars=strsplit(temp,"+",fixed=TRUE)[[1]]

        if(length(yvars)!=2) {
                cat("The formula should be : time+status~1\n")
                return(NULL)
        }
        timevar=yvars[1]
        statusvar=yvars[2]


     if(is.null(ylabs)) ylabs=paste0("Probability of death from ",id)
     if(is.null(xlab)) xlab=paste0("Time from " ,id[1]," diagnosis")
     if(is.null(label)) labels=paste0("Death from\n",id)


     df1=map2_dfr(1:length(id),id,function(x,y){
          myformula=paste0("survival::Surv(",timevar,",",statusvar,"==",x,")~1")
          fit=survfit(as.formula(myformula),data=data)
          if(x==2){
               est=fit$surv
               se=fit$std.err
          }else{
               est=1-fit$surv
               se=1-fit$std.err
          }

          df=data.frame(time=fit$time,est=est,se=se) %>%
               mutate(lower=est-1.96*se,
                      upper=est+1.96*se,
                      id=y,
                      method="KM")
          df
     })

     df3=data.frame(label=labels,x=xpos,y=ypos,id=id)

     formula=paste0("survival::Surv(",timevar,",",statusvar,")~1")
     formula
     if(!is.factor(data[[statusvar]])){
          data[[statusvar]]=factor(data[[statusvar]])
     }
     suppressWarnings(fit1<-cuminc(as.formula(formula),data=data))

     df2=fit1$tidy[c(1,3,4,5,6,2)]
     names(df2)=c("time","est","se","lower","upper","id")
     df2$est[df2$id!=1]=1-df2$est[df2$id!=1]
     df2$se[df2$id!=1]=1-df2$se[df2$id!=1]
     df2$lower[df2$id!=1]=1-df2$lower[df2$id!=1]
     df2$upper[df2$id!=1]=1-df2$upper[df2$id!=1]
     df2$id=factor(df2$id,labels=id)
     df2$method="CIC"

     df=rbind(df1,df2)


     p<-ggplot(df,aes_string(x="time",y="est"))
     if(se) p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="id",linetype="method"),alpha=0.2)
     p=p+ geom_step(aes_string(color="id",linetype="method"))+theme_bw()+
          scale_y_continuous(
               sec.axis=sec_axis(~1-.,name=ylabs[2]))+
          labs(x=xlab,
               y=ylabs[1])+
          theme(legend.position="bottom",panel.grid=element_blank())+
          guides(color="none")+
          geom_text(data=df3,aes_string(x="x",y="y",label="label"))

     if(plot) print(p)

     invisible(list(df,df3,p))
}

