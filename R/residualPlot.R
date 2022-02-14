#'Draw a residual plot with an object of class coxph
#'@param fit An object of class coxph
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
residualPlot=function(fit,type="martingale",vars=NULL,ncol=2,show.point=TRUE,se=TRUE,topn=5,labelsize=4){
       # type="partial"
     #type="partial";vars=NULL;show.point=TRUE;se=TRUE
     data=fit2model(fit)
     r1=residuals(fit,type=type)
     xvars=attr(fit$term,"term.labels")
     if(!is.null(vars)) {  xvars=intersect(xvars,vars)}
     if(type %in% c("score","schoenfeld","dfbeta","dfbetas","scaledsch")){
     xvars2 = attr(fit$coefficients, "names")
     xvars=xvars2[unlist(map(xvars,~fit$assign[[.]]))]
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
          if(type!="partial") colnames(r1)=xvars2
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
                    p=p+ labs(y=paste0(type,"(",x,")"))+
                         theme_classic()
                    colnames(r1)[colnames(r1)=="y"]=x
                    p
               # }
          })

          do.call(wrap_plots,list(p,ncol=ncol))+
               plot_annotation(title=paste0(type," residual plot"))

     } else{


     data$r1=r1
     data
     if(length(xvars)==1) ncol=1
     p=map(xvars,function(x){
              if(is.mynumeric(data[[x]])){
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
                plot_annotation(title=paste0(type," residual plot"))
     }
}
