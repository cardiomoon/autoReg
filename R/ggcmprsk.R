#'Draw Cumulative Incidence Curves for Competing Risks
#'@param x A formula as time+status~1
#'@param data A data.frame
#'@param id character vector label for status
#'@param strata  character vector label for strata
#'@param se logical whether or not show confidence interval
#'@param facet numeric if facet is not NULL, draw plot with selected facets
#'@param ... Further arguments to be passed to tidycmprsk::cuminc
#'@importFrom tidycmprsk cuminc
#'@importFrom purrr map2_dfr
#'@importFrom survival Surv
#'@importFrom ggplot2 geom_line geom_ribbon scale_color_discrete theme_classic
#'@examples
#'data(melanoma,package="boot")
#'melanoma$status1 = ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#'ggcmprsk(time/365+status1~1,data=melanoma)
#'ggcmprsk(time/365+status1~1,data=melanoma,id=c("alive","melanoma","other"),se=TRUE)
#'ggcmprsk(time/365+status1~sex,data=melanoma)
#'ggcmprsk(time/365+status1~sex,data=melanoma,facet=1)
#'ggcmprsk(time/365+status1~sex,data=melanoma,
#'id=c("alive","melanoma","other"),strata=c("female","male"))
#'ggcmprsk(time/365+status1~sex,data=melanoma,
#'id=c("alive","melanoma","other"),strata=c("female","male"),facet=1)
#'@return An object of class "ggplot"
#'@export
ggcmprsk=function(x,data,id=NULL,se=FALSE,strata=NULL,facet=NULL,...){
         # x=time/365+status1~sex;data=melanoma;id=NULL;se=FALSE;facet=NULL;strata=NULL
       # strata=c("female","male")
     temp=strsplit(deparse(x),"~")[[1]][1]
     temp=gsub(" ","",temp)
     myt=terms(x,data=data)
     xvars=attr(myt,"term.labels")
     if(length(xvars)==1){
          if(!is.null(strata)) data[[xvars]]=factor(data[[xvars]],labels=strata)
     } else if(length(xvars)==0) xvars="1"
     yvars=strsplit(temp,"+",fixed=TRUE)[[1]]

     if(length(yvars)!=2) {
          cat("The formula should be : time+status~1\n")
          return(NULL)
     }
     timevar=yvars[1]
     statusvar=yvars[2]
     if(!is.factor(data[[statusvar]])) {
          if(!is.null(id)) {
               data[[statusvar]]=factor(data[[statusvar]],labels=id)
          } else{
               data[[statusvar]]=factor(data[[statusvar]])
          }
     }
     formula=paste0("survival::Surv(",timevar,",",statusvar,")~",paste0(xvars,collapse="+"))
         # suppressWarnings(x<-cuminc(as.formula(formula),data=data))
     suppressWarnings(x<-cuminc(as.formula(formula),data=data,...))
     df=x$tidy
     if(!is.null(facet)){

          if(!is.null(id)) {
               df=df[df$outcome %in% id[facet+1],]
          } else{
               df=df[df$outcome %in% c(facet),]
          }
     }
     add.p=FALSE
     if(!is.null(x$cmprsk$Tests)){
          dfp=data.frame(p=p2character2(round(x$cmprsk$Tests[,2],3),add.p=TRUE),
                         outcome=factor(x$failcode,labels=names(x$failcode)))
          add.p=TRUE
          if(!is.null(facet)){
          if(!is.null(id)) {
               dfp=dfp[dfp$outcome %in% id[facet+1],]
          } else{
               dfp=dfp[dfp$outcome %in% c(facet),]
          }
          }
     }

     if(add.p){
          p=ggplot(df,aes_string(x="time",y="estimate"))+
               geom_step(aes_string(color="strata"))
          if(se) p=p+geom_stepribbon(aes_string(ymin="conf.low",ymax="conf.high",fill="strata"),alpha=0.2)
          if(add.p) p=p+geom_text(data=dfp,aes(x=-Inf,y=Inf,label=p),hjust=-0.2,vjust=2)
          p=p+ facet_wrap(~outcome)

     } else{
          p=ggplot(x$tidy,aes_string(x="time",y="estimate"))+
               geom_step(aes_string(color="outcome"))
          if(se) p=p+geom_stepribbon(aes_string(ymin="conf.low",ymax="conf.high",fill="outcome"),alpha=0.2)

     }
     p=p+ labs(x="time",y="Proportion of event")+
          theme_classic()+
          theme(legend.position="top",
                panel.background = element_rect(colour="black"))
     p

     # if(is.null(id)) id=names(fit)
     # result=map2_dfr(fit,id,function(x,y){
     #      x=as.data.frame(x)
     #      x %>% mutate(
     #           upper=.data$est+1.96*sqrt(.data$var),
     #           lower=.data$est-1.96*sqrt(.data$var),
     #           name=y
     #      )
     # })
     #
     # p=ggplot(result,aes_string(x="time",y="est"))+
     #      geom_step(aes_string(color="name"))
     # if(se) p=p+geom_stepribbon(aes_string(ymin="lower",ymax="upper",fill="name"),alpha=0.2)
     # p= p+ labs(x="Time",y="Probability of an event",title="Cumulative incidence Function")+
     #      theme_classic()+
     #      theme(legend.position="top")+
     #      scale_color_discrete(name="")+
     #      guides(fill="none")
     # p
}



