#'Compare cumulative incidence to th Kaplan-Meier estimate
#'@param timevar a name of time variable
#'@param statusvar a name of status variable. coded as one of c(0,1,2)
#'@param data A data.frame
#'@param id Character vector of length2
#'@param xpos numeric x-axis position of label
#'@param ypos numeric y-axis position of label
#'@param ylabs string vector of length 2. y axis labels
#'@param xlab A character. The x-axis label
#'@param label string vector of length 2. Label names
#'@param plot logical Whether or not print plot
#'@importFrom cmprsk cuminc
#'@importFrom survival survfit Surv
#'@importFrom ggplot2 scale_y_continuous sec_axis
#'@return A list
#'\describe{
#'   \item{df}{A long data.frame consist of time, est, upper,lower, id, method}
#'   \item{df3}{A data.frame for label consist of x, y, label, id}
#'   \item{p}{A ggplot object}
#'}
#'@export
#'@examples
#'require(dplyr)
#'data(prostateSurvival,package="asaur")
#'prostateHighRisk <- prostateSurvival %>%
#'   filter(grade=="poor" & stage=="T2",ageGroup=="80+")
#'ggcmprisk2("survTime/12","status",data=prostateHighRisk,
#'      id=c("prostate cancer","other causes"))
ggcmprisk2=function(timevar,statusvar,data,id=c("disease","other"),
                    xpos=c(2,2),ypos=c(0.25,0.70),
                    ylabs=NULL,xlab=NULL,label=NULL,plot=TRUE){

     if(is.null(ylabs)) ylabs=paste0("Probability of death from ",id)
     if(is.null(xlab)) xlab=paste0("Time from " ,id[1]," diagnosis")
     if(is.null(label)) labels=paste0("Death from\n",id)

     time=eval(parse(text=paste0("data$",timevar)))
     status=eval(parse(text=paste0("data$",statusvar)))

     df1=map2_dfr(1:length(id),id,function(x,y){
          fit=survfit(Surv(time,status==x)~1)
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

     fit1=cuminc(time,status)

     df2=map2_dfr(fit1,id,function(x,y){
          if(y==id[1]){
               est=x$est
               se=x$var
          }else{
               est=1-x$est
               se=1-x$var
          }
          df=data.frame(time=x$time,
                        est=est,
                        se=se) %>%
               mutate(
                    lower=est-1.96*se,
                    upper=est+1.96*se,
                    id=y,
                    method="cif")
          df
     })

     df2
     df=rbind(df1,df2)


     p<-ggplot(df,aes_string(x="time",y="est"))+
          geom_line(aes_string(color="id",linetype="method"))+theme_bw()+
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

