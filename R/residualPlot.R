#'Draw a residual plot with an object of class coxph
#'@param fit An object of class coxph
#'@param type character One of the c("martingale","deviance","score","schoenfeld",
#'"dfbeta","dfbetas","scaledsch","partial"). Default value is "martingale".
#'@param vars character Names of variables to plot. default value is NULL
#'@param show.point logical Whether or not show point
#'@param se logical Whether or not show se
#'@param ncol numeric number of columns
#'@importFrom patchwork wrap_plots plot_annotation
#'@importFrom graphics legend lines
#'@importFrom stats residuals
#'@importFrom ggplot2 stat_smooth geom_boxplot geom_jitter
#'@importFrom dplyr as_tibble semi_join
#'@return A patchwork object
#'@export
#'@examples
#'require(survival)
#'data(cancer)
#'fit=coxph(Surv(time,status==2)~log(bili)+age+cluster(edema),data=pbc)
#'residualPlot(fit)
#'residualPlot(fit,var="age")
#'fit=coxph(Surv(time,status==2)~age,data=pbc)
#'residualPlot(fit)
#'residualPlot(fit,"partial")
#'fit=coxph(Surv(time,status)~rx+sex+logWBC,data=anderson)
#'residualPlot(fit,ncol=3)
residualPlot=function(fit,type="martingale",vars=NULL,ncol=2,show.point=TRUE,se=TRUE){
       # type="partial"
     #type="martingale";vars=NULL;show.point=TRUE;se=TRUE
     data=fit2model(fit)
     r1=residuals(fit,type=type)
     xvars=attr(fit$term,"term.labels")
     if(!is.null(vars)) xvars=intersect(xvars,vars)

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
          r1=as_tibble(r1)
          colnames(r1)=xvars2
          r1$index=1:nrow(r1)
          r1
          xvars2

          p=map(xvars2,function(x){

               if(x %in% xvars){
                   df=data.frame(x=data[[x]],y=r1[[x]])

                   if(is.mynumeric(df$x)){
                        p=ggplot(df,aes_string(x="x",y="y"))
                        if(show.point) p=p+geom_point(alpha=0.3)
                        if(se) p=p+stat_smooth(method="loess",formula="y~x")
                        p=p+labs(y=type,x=x)+
                             theme_classic()
                        p
                   } else{
                        if(is.numeric(df$x)){
                             df$x=factor(df$x)
                        }
                        p=ggplot(df,aes_string(x="x",y="y"))+
                             geom_boxplot()
                        if(show.point) p=p+geom_jitter(width=0.2,alpha=0.1)
                        p=p+labs(y=type,x=x)+
                             theme_classic()
                        p
                   }
               } else{

                    colnames(r1)[colnames(r1)==x]="y"
                    p=ggplot(data=r1,aes_string(x="index",y="y"))
                    if(show.point) p=p+ geom_point(alpha=0.2)
                    if(se) p=p+ stat_smooth(method="loess",formula="y~x")
                    p=p+ labs(y=paste0(type,"(",x,")"))+
                         theme_classic()
                    colnames(r1)[colnames(r1)=="y"]=x
                    p
               }
          })
          do.call(wrap_plots,p)+
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
