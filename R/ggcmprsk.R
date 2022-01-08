#'Draw Cumulative Incidence Curves for Competing Risks
#'@param x A formula as time+status~1
#'@param data A data.frame
#'@param id character vector
#'@param se logical whether or not show confidence interval
#'@importFrom cmprsk cuminc
#'@importFrom purrr map2_dfr
#'@importFrom ggplot2 geom_line geom_ribbon scale_color_discrete theme_classic
#'@examples
#'data(melanoma,package="boot")
#'melanoma$status1 = ifelse(melanoma$status==1,1,ifelse(melanoma$status==2,0,2))
#'ggcmprsk(time/365+status1~1,data=melanoma,id=c("melanoma","other"))
#'@return An object of class "ggplot"
#'@export
ggcmprsk=function(x,data,id=NULL,se=FALSE){

        temp=strsplit(deparse(x),"~")[[1]][1]
        temp=gsub(" ","",temp)
        yvars=strsplit(temp,"+",fixed=TRUE)[[1]]

        if(length(yvars)!=2) {
                cat("The formula should be : time+status~1\n")
                return(NULL)
        }
     time=eval(parse(text=paste0("data$",yvars[1])))
     status=eval(parse(text=paste0("data$",yvars[2])))

     fit=cuminc(time,status)
     if(is.null(id)) id=names(fit)
     result=map2_dfr(fit,id,function(x,y){
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



