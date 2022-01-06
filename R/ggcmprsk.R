#'Draw Cumulative Incidence Curves for Competing Risks
#'@param fit An  object of class cuminc
#'@param cnames character vector
#'@param se logical whether or not show confidence interval
#'@importFrom purrr map2_dfr
#'@importFrom ggplot2 geom_line geom_ribbon scale_color_discrete theme_classic
#'@examples
#'library(cmprsk)
#'data(melanoma,package="boot")
#'melanoma$status1 = ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#'fit=cuminc(melanoma$time/365,melanoma$status1)
#'ggcmprsk(fit,c("melanoma","others"))
#'@export
ggcmprsk=function(fit,cnames=NULL,se=FALSE){

     if(is.null(cnames)) cnames=names(fit)
     result=map2_dfr(fit,cnames,function(x,y){
          x=as.data.frame(x)
          x %>% mutate(
               upper=.data$est+1.96*sqrt(.data$var),
               lower=.data$est-1.96*sqrt(.data$var),
               name=y
          )
     })

     p=ggplot(result,aes_string(x="time",y="est"))+
          geom_line(aes_string(color="name"))
     if(se) p=p+geom_ribbon(aes_string(ymin="lower",ymax="upper",fill="name"),alpha=0.2)
     p= p+ labs(x="Time",y="Probability of an event",title="Cumulative incidence Function")+
          theme_classic()+
          theme(legend.position="top")+
          scale_color_discrete(name="")+
          guides(fill="none")
     p
}



