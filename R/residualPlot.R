#'Draw a residual plot with an object of class coxph
#'@param fit An object of class coxph or survreg
#'@param type character One of the c("martingale","deviance","score","schoenfeld",
#'"dfbeta","dfbetas","scaledsch","partial"). Default value is "martingale".
#'@param vars character Names of variables to plot. default value is NULL
#'@param show.point logical Whether or not show point
#'@param se logical Whether or not show se
#'@param ncol numeric number of columns
#'@param topn numeric number of data to be labelled
#'@param labelsize numeric size of label
#'@importFrom patchwork wrap_plots plot_annotation
#'@importFrom graphics legend lines
#'@importFrom stats residuals
#'@importFrom ggplot2 stat_smooth geom_boxplot geom_jitter geom_segment geom_hline geom_label
#'@importFrom dplyr as_tibble semi_join desc arrange
#'@importFrom grid unit
#'@importFrom purrr reduce
#'@return A patchwork object
#'@export
#'@examples
#'require(survival)
#'data(cancer)
#'fit=coxph(Surv(time,status==2)~log(bili)+age+cluster(edema),data=pbc)
#'residualPlot(fit)
#'residualPlot(fit,vars="age")
#'fit=coxph(Surv(time,status==2)~age,data=pbc)
#'residualPlot(fit)
#'residualPlot(fit,"partial")
#'fit=coxph(Surv(time,status)~rx+sex+logWBC,data=anderson)
#'residualPlot(fit,ncol=3)
#'\dontrun{
#'data(pharmacoSmoking,package="asaur")
#'fit=coxph(Surv(ttr,relapse)~grp+employment+age,data=pharmacoSmoking)
#'residualPlot(fit)
#'residualPlot(fit,var="age")
#'residualPlot(fit,type="dfbeta")
#'residualPlot(fit,type="dfbeta",var="age")
#'residualPlot(fit,type="dfbeta",var="employment")
#'residualPlot(fit,type="dfbeta",var="employmentother")
#'pharmacoSmoking$ttr[pharmacoSmoking$ttr==0]=0.5
#'fit=survreg(Surv(ttr,relapse)~grp+age+employment,data=pharmacoSmoking,dist="weibull")
#'residualPlot(fit,type="response")
#'residualPlot(fit,type="deviance")
#'residualPlot(fit,type="dfbeta",vars="age")
#'fit=survreg(Surv(time,status)~ph.ecog+sex*age,data=lung,dist="weibull")
#'residualPlot(fit,"dfbeta")
#'residualPlot(fit,"deviance")
#'}
residualPlot=function(fit,type="martingale",vars=NULL,ncol=2,show.point=TRUE,se=TRUE,topn=5,labelsize=4){
       # type="partial"
     #type="deviance";vars=NULL;show.point=TRUE;se=TRUE;topn=5;labelsize=4
     data=fit2model(fit)
     if("survreg" %in% class(fit)) {
          if(type=="martingale") {type="response"}
     }
     xvars=attr(fit$term,"term.labels")
     xvars2=setdiff(xvars,xvars[str_detect(xvars,":")])
     narows=which(apply(data[xvars2],1,function(x) {any(is.na(x))}))
     narows
     if(length(narows)>0) data=data[-narows,]
     r1=residuals(fit,type=type)
     if(!is.null(vars)) {
          xvars2 = attr(fit$coefficients, "names")
          xvars=intersect(union(xvars,xvars2),vars)
     }
     if(type %in% c("score","schoenfeld","dfbeta","dfbetas","scaledsch")){
        xvars2 = attr(fit$coefficients, "names")
        xvars=unlist(map(xvars,function(x) {
             if(x %in% xvars2){
                  x
             } else if("coxph" %in% class(fit)){
                  xvars2[fit$assign[[x]]]
             } else if("survreg" %in% class(fit)){
                  paste0(x,fit$xlevels[[x]][-1])
             }
        }))
     }


     y = as.character(fit$formula)[2]
     temp1=str_remove_all(y,"Surv\\(|\\)| ")
     temp1=unlist(strsplit(temp1,","))
     timevar=temp1[1]
     statusvar=temp1[2]

     class(r1)
     if(type=="schoenfeld"){

          if(str_detect(statusvar,"==")){
               data=data[eval(parse(text=paste0("data$",statusvar))),]
          } else{
               data=data[data[[statusvar]]==1,]
          }
     }
     if("matrix" %in% class(r1)){

          xvars2 = attr(fit$coefficients, "names")
          r1=as_tibble(r1,.name_repair="minimal" )
          if(type!="partial") {
               if("survreg" %in% class(fit)) {
                    colnames(r1)=c(xvars2,"Log(scale)")
               } else{
                    colnames(r1)=xvars2
               }
          }

          r1$index=1:nrow(r1)
          r1
          xvars2
          if(length(xvars)==1) ncol=1

          p=map(xvars,function(x){

               # if(x %in% xvars){
               #     df=data.frame(x=data[[x]],y=r1[[x]])
               #
               #     if(is.mynumeric(df$x)){
               #          p=ggplot(df,aes_string(x="x",y="y"))
               #          if(type %in% c("dfbeta","dfbetas")){
               #             p=p+geom_segment(aes_string(xend="x",yend="0"))+
               #                  geom_hline(yintercept=0)
               #          } else{
               #          if(show.point) p=p+geom_point(alpha=0.3)
               #          if(se) p=p+stat_smooth(method="loess",formula="y~x")
               #          }
               #          p=p+labs(y=paste0(type,"(",x,")"))+
               #               theme_classic()
               #          p
               #     } else{
               #          if(is.numeric(df$x)){
               #               df$x=factor(df$x)
               #          }
               #          p=ggplot(df,aes_string(x="x",y="y"))+
               #               geom_boxplot()
               #          if(show.point) p=p+geom_jitter(width=0.2,alpha=0.1)
               #          p=p+labs(y=paste0(type,"(",x,")"),x=index)+
               #               theme_classic()
               #          p
               #     }
               # } else{

                    colnames(r1)[colnames(r1)==x]="y"
                    r2=r1
                    r2$absy=abs(r2$y)
                    r2 %>% arrange(desc(.data$absy)) ->r2
                    r2=r2[1:topn,]
                    r2$percent=r2$absy*100/abs(fit$coefficients[x])
                    r2$color=ifelse(r2$percent>=10,"red","black")
                    if(type=="dfbetas") r2$color="black"
                    p=ggplot(data=r1,aes_string(x="index",y="y"))
                    if(type %in% c("dfbeta","dfbetas")){
                         p=p+geom_segment(aes_string(xend="index",yend="0"))+
                              geom_hline(yintercept=0)+
                              geom_label(data=r2,aes_string(label="index"),
                                         label.padding=unit(0.15,"lines"),
                                         label.r=unit(0.1,"lines"),
                                         label.size=0.1,
                                         size=labelsize,
                                         color=r2$color)
                    } else{
                    if(show.point) p=p+ geom_point(alpha=0.2)
                    if(se) p=p+ stat_smooth(method="loess",formula="y~x")
                    }
                    p=p+ labs(y=paste("Change in coefficient for",x))+
                         theme_classic()
                    colnames(r1)[colnames(r1)=="y"]=x
                    p
               # }
          })

          do.call(wrap_plots,list(p,ncol=ncol))+
               plot_annotation(title=paste("Case deletion",type,"residuals"),
                               theme=theme(plot.title=element_text(size=16,hjust=0.5)))

     } else{


     data$r1=r1
     data
     if(length(xvars)==1) ncol=1
     p=map(xvars,function(x){
              if(str_detect(x,":")){
                 temp=unlist(strsplit(x,":"))
                 data$x=reduce(data[temp],`*`)
                 p=ggplot(data,aes_string(x="x",y="r1"))
                 if(show.point) p=p+geom_point(alpha=0.3)
                 if(se) p=p+ stat_smooth(method="loess",formula="y~x")
                 p=p+ labs(y=paste0(type," residual"),
                           x=paste0(temp,collapse=":"))+
                      theme_classic()
                 p

              } else if(is.mynumeric(data[[x]])){
                   p=ggplot(data,aes_string(x=x,y="r1"))
                   if(show.point) p=p+geom_point(alpha=0.3)
                   if(se) p=p+ stat_smooth(method="loess",formula="y~x")
                   p=p+ labs(y=paste0(type," residual"))+
                        theme_classic()
                   p

              } else{
                   if(is.numeric(data[[x]])){
                        data[[x]]=factor(data[[x]])
                   }
                   p=ggplot(data,aes_string(x,y="r1"))+
                        geom_boxplot()
                  if(show.point) p=p+ geom_jitter(width=0.2,alpha=0.1)
                  p=p+ labs(y=paste0(type," residual"))+
                        theme_classic()
                  p
              }
          })

          do.call(wrap_plots,list(p,ncol=ncol))+
                plot_annotation(title=paste0(type," residual plot"),
                                theme=theme(plot.title=element_text(size=16,hjust=0.5)))
     }
}
