#' Show effects of covariates
#' @param fit An object of class survreg
#' @param x character name of x-axis variable
#' @param color character name of color variable
#' @param facet character name of facet variable
#' @param autovar logical Whether or not select color and facet variable automatically
#' @param pred.values list list of values of predictor variables
#' @param se logical whether or not show se
#' @param logy logical WHether or not draw y-axis on log scale
#' @param collabel labeller for column
#' @param rowlabel labeller for row
#' @importFrom ggplot2 geom_errorbar position_dodge labeller label_both label_value
#' @return A ggplot
#' @export
#' @examples
#' library(survival)
#' library(ggplot2)
#' fit=survreg(Surv(time,status)~ph.ecog+sex*age,data=lung,dist="weibull")
#' showEffect(fit)
#' fit=survreg(Surv(time,status)~rx+sex+age+obstruct+adhere,data=colon,dist="weibull")
#' showEffect(fit)
#' showEffect(fit,rowlabel=label_value)
#' fit=survreg(Surv(time,status)~ph.ecog+sex,data=lung,dist="weibull")
#' showEffect(fit)
#' fit=survreg(Surv(time,status)~ph.ecog+age,data=lung,dist="weibull")
#' showEffect(fit)
#' fit=survreg(Surv(time,status)~sex*age,data=lung,dist="weibull")
#' showEffect(fit)
#' fit=survreg(Surv(time,status)~age,data=lung,dist="weibull")
#' showEffect(fit)
showEffect=function(fit,x=NULL,color=NULL,facet=NULL,autovar=TRUE,pred.values=list(),se=TRUE,logy=TRUE,collabel=label_both,rowlabel=label_both){
              # x=NULL;color=NULL;facet=NULL;pred.values=list()  ;se=TRUE;logy=TRUE;collabel=label_both;rowlabel=label_both;autovar=TRUE
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     xvars=xvars[!str_detect(xvars,":")]
     x2<-color2<-facet2<-NULL
     for(i in seq_along(xvars)){
          if(is.mynumeric(data[[xvars[i]]])){
               if(is.null(x2)) x2=xvars[i]
          } else{
               if(is.null(color)) {
                    color2=xvars[i]
               } else if(is.null(facet)){
                    facet2=xvars[i]
               } else{
                    facet2=unique(c(facet2,xvars[i]))
                    facet2=setdiff(facet2,c(color2,x2))
                    if(length(facet2)>2){
                         facet2=facet2[1:2]
                    }

               }
          }
     }
     if(is.null(x)) { x<-x2}
     if(autovar){
     if(is.null(color)) { color<-color2}
     if(is.null(facet)) { facet<-facet2}
     }
     x;color;facet;
     if(is.null(x)){
          if(!is.null(color)) {
               x=color
               if(!is.null(facet)) {
                    color=facet[1]
                    facet=facet[-1]
               } else{
                    color=NULL
               }

          }
     }
     if(is.mynumeric(data[[x]])){
          xvalues=seq(min(data[[x]],na.rm=TRUE),max(data[[x]],na.rm=TRUE))
     } else{
          xvalues=sort(unique(data[[x]]))

     }

     temp=c(color,facet)
     temp
     res=map(temp,~sort(unique(data[[.]])))
     res[["x"]]=xvalues
     res
     names(res)=c(color,facet,x)
     add=setdiff(xvars,c(color,facet,x))
     for(i in seq_along(add)){
          if(is.mynumeric(data[[add[i]]])){
               res[[add[i]]]=mean(data[[add[i]]],na.rm=TRUE)
          } else{
               res[[add[i]]]=names(sort(table(data[[add[i]]]),decreasing=TRUE))[1]
               if(is.numeric(data[[add[i]]])) res[[add[i]]]=as.numeric(res[[add[i]]])
     }
     }
     newdata=expand.grid(res)
     newdata
     result=predict(fit,newdata=newdata,se.fit=TRUE)
     newdata$est=result$fit
     newdata$se=result$se.fit

     newdata$lower=newdata$est-1.96*newdata$se
     newdata$upper=newdata$est+1.96*newdata$se
     for(i in seq_along(temp)){
          if(is.numeric(newdata[[temp[i]]])){
               newdata[[temp[i]]]=factor(newdata[[temp[i]]])
          }
     }
     if(!is.mynumeric(data[[x]])){
          newdata[[x]]=factor(newdata[[x]])
     }

     myformula=NULL
     if(length(facet)==1) {
          myformula=paste0("~",facet)
     } else if(length(facet)==2) {
          myformula=paste0(facet[1],"~",facet[2])
     }

     p=ggplot(newdata,aes_string(x=x,y="est"))
     if(is.mynumeric(data[[x]])){
          if(!is.null(color)){
               p=p+geom_line(aes_string(color=color))
               if(se) p=p+ geom_ribbon(aes_string(ymin="lower",ymax="upper",fill=color),alpha=0.3)
          } else{
               p=p+geom_line()
               if(se) p=p+ geom_ribbon(aes_string(ymin="lower",ymax="upper"),alpha=0.3)

          }
     } else{
          if(is.null(color)){
               p=p+geom_point()
               if(se) p=p+ geom_errorbar(aes_string(ymin="lower",ymax="upper"),width=0.1)
          } else{
               p=p+geom_point(aes_string(color=color),position=position_dodge(width=0.2))
               if(se) p=p+ geom_errorbar(aes_string(ymin="lower",ymax="upper",color=color),
                                         position=position_dodge(width=0.2),width=0.1)
          }
     }
     if(!is.null(myformula)) {
          if(length(facet)==1){
            p=p+facet_wrap(as.formula(myformula),
                           labeller="label_both")
          } else{
               p=p+facet_grid(as.formula(myformula),
                              labeller=labeller(.rows=rowlabel,.cols=collabel))
          }
     }
     p=p+theme_classic()+
          labs(y="Days")+
          theme(panel.border = element_rect(fill=NA))
     if(logy) p=p+scale_y_log10()
     if(length(add)>0){
          res=map_chr(add,function(x){
               temp=res[[x]]
               if(is.numeric(temp)) temp=round(temp,2)
               paste0(x,"=",temp)
          })
          p=p+labs(subtitle=paste0(res,collapse=","))
     }
     p
}
