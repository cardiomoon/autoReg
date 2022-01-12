#'Draw a residual plot with an object of class coxph
#'@param fit An object of class coxph
#'@param type character One of the c("martingale","deviance","score","schoenfeld",
#'"dfbeta","dfbetas","scaledsch","partial"). Default value is "martingale".
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
#'fit=coxph(Surv(time,status==2)~age,data=pbc)
#'residualPlot(fit)
#'residualPlot(fit,"partial")
residualPlot=function(fit,type="martingale"){
       # type="partial"
     data=fit2model(fit)
     r1=residuals(fit,type=type)
     xvars=attr(fit$term,"term.labels")

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
                        ggplot(df,aes_string(x="x",y="y"))+
                             geom_point(alpha=0.3)+
                             stat_smooth(method="loess",formula="y~x")+
                             labs(y=type,x=x)+
                             theme_classic()
                   } else{
                        if(is.numeric(df$x)){
                             df$x=factor(df$x)
                        }
                        ggplot(df,aes_string(x="x",y="y"))+
                             geom_boxplot()+
                             geom_jitter(width=0.2,alpha=0.1)+
                             labs(y=type,x=x)+
                             theme_classic()
                   }
               } else{

                    colnames(r1)[colnames(r1)==x]="y"
                    p=ggplot(data=r1,aes_string(x="index",y="y"))+
                         geom_point(alpha=0.2)+
                         stat_smooth(method="loess",formula="y~x")+
                         labs(y=paste0(type,"(",x,")"))+
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
     p=map(xvars,function(x){
              if(is.mynumeric(data[[x]])){
                   ggplot(data,aes_string(x=x,y="r1"))+
                        geom_point(alpha=0.3)+
                        stat_smooth(method="loess",formula="y~x")+
                        labs(y=paste0(type," residual"))+
                        theme_classic()

              } else{
                   if(is.numeric(data[[x]])){
                        data[[x]]=factor(data[[x]])
                   }
                   ggplot(data,aes_string(x,y="r1"))+
                        geom_boxplot()+
                        geom_jitter(width=0.2,alpha=0.1)+
                        labs(y=paste0(type," residual"))+
                        theme_classic()
              }
          })

          do.call(wrap_plots,p)+
                plot_annotation(title=paste0(type," residual plot"))
     }
}
