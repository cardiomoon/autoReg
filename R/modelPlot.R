#' Draw coefficients/odds ratio/hazard ratio plot
#' @param fit An object of calss glm
#' @param widths Numeric vector
#' @param change.pointsize logical Whether or not change point size
#' @param show.OR logical Whether or not show odds ratio
#' @param show.ref logical Whether or not show reference
#' @param bw logical If true, use grey scale
#' @param legend.position legend position default value is 'top'
#' @param ... Further arguments to be passed to autoReg()
#' @importFrom ggplot2 ggplot geom_vline geom_point geom_errorbar labs scale_y_discrete position_dodge
#' guides theme_bw theme element_text element_blank geom_text ggtitle aes xlab ylab aes_string
#' scale_fill_grey scale_color_grey
#' @importFrom patchwork plot_layout
#' @importFrom stats reorder
#' @importFrom dplyr full_join
#' @importFrom stringr str_replace_all
#' @return modelPlot returns an  object of class "modelPlot"
#' An object of class modelPlot is a list containing at least of the following components:
#' \describe{
#'    \item{tab1}{The first table containing names}
#'    \item{tab2}{The 2nd table containing levels}
#'    \item{tab3}{The 3rd table containing coefficients or odds ratio or hazards ratio}
#'    \item{p}{A ggplot}
#'    \item{widths}{the widths of the tables and the ggplot}
#' }
#' @examples
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' modelPlot(fit,widths=c(1,0,2,3))
#' modelPlot(fit,uni=TRUE,threshold=1,widths=c(1,0,2,3))
#' fit=lm(Sepal.Width~Sepal.Length*Species,data=iris)
#' modelPlot(fit)
#' modelPlot(fit,uni=TRUE,change.pointsize=FALSE)
#' \donttest{
#' data(cancer,package="survival")
#' fit=glm(status~rx+age+sex+nodes+obstruct+perfor,data=colon,family="binomial")
#' modelPlot(fit)
#' modelPlot(fit,uni=TRUE,multi=TRUE,threshold=1)
#' modelPlot(fit,multi=TRUE,imputed=TRUE,change.pointsize=FALSE)
#' data(colon_s,package="finalfit")
#' fit=glm(mort_5yr~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s,family="binomial")
#' modelPlot(fit)
#' modelPlot(fit,uni=TRUE,multi=TRUE,threshold=1)
#' modelPlot(fit,uni=TRUE,multi=TRUE)
#' modelPlot(fit,uni=TRUE,multi=TRUE,threshold=1,show.ref=FALSE)
#' library(survival)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' modelPlot(fit)
#' modelPlot(fit,uni=TRUE,threshold=1)
#' fit=coxph(Surv(time,status)~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s)
#' modelPlot(fit)
#' modelPlot(fit,uni=TRUE,threshold=1)
#' modelPlot(fit,uni=TRUE,threshold=1,show.ref=FALSE)
#' modelPlot(fit,imputed=TRUE)
#' }
#' @export
modelPlot=function(fit,widths=NULL,change.pointsize=TRUE,show.OR=TRUE,show.ref=TRUE,bw=TRUE,
                   legend.position="top",...){
         # fit=lm(Sepal.Width~Sepal.Length*Species,data=iris)
           # fit=lm(NTAV~age*sex+I(age^2)*sex,data=radial)
            # widths=NULL;change.pointsize=TRUE;uni=TRUE;multi=TRUE;imputed=FALSE;show.OR=TRUE;
            # show.ref=TRUE;bw=TRUE;legend.position="top"

     if(is.null(widths)) {
        if(show.OR) {
          widths=c(1.2,1,2,3.5)
        } else{
          widths=c(1.2,1,2,3.5)
        }
     }

     mode=1
     xname="Estimate"
     xlabel="Coefficient (95% CI)"
     if("glm" %in% class(fit)){
          mode=2
          xname="OR"
          xlabel="Odds Ratio (95% CI)"
     } else if("coxph" %in% class(fit)){
             mode=3
             xname="HR"
             xlabel="Hazard Ratio (95% CI)"
     }
     xvars=attr(fit$term,"term.labels")
     xvars
     data=fit2model(fit)

     others=setdiff(xvars,names(data))
     xvars=setdiff(xvars,others)
     if(length(xvars)>0){

     myformula=paste0("~",paste0(xvars,collapse="+"))
     df1=gaze(as.formula(myformula),data=data,show.n=TRUE)
     df1$id=str_replace_all(df1$id,"`","")
     df1$name=str_replace_all(df1$name,"`","")
     plusminus="\u00b1"
     df1$desc[df1$desc==paste0("Mean ",plusminus," SD")]=""
     } else{
       df1=data.frame(name="",desc="",N=NA,stats="",n=1,id="")
       df1=df1[-1,]
       df1
     }

     del=str_detect(others,"strata\\(|cluster\\(|frailty\\(")
     if(any(del)) others=others[-which(del)]

     if(length(others)>0){   ## interactions or interpretations
          for(i in 1:length(others)){
                     # i=1
               name=others[i]
               desc="others"
               if(str_detect(name,":")) {
                    temp=getInteraction(name,data=data)
                    temp$N=temp$n
                    temp$stats=""
                    temp=temp[c(1,2,5,6,4,3)]


               } else if(str_detect(name,fixed("I("))){
                    desc="interpretation"
                    id=name
                    n=nrow(data)
                    N=NA
                    temp=data.frame(name=name,desc=desc,N=N,stats="",n=n,id=id)
               }
               class(df1)="data.frame"
               tempname=setdiff(names(df1),names(temp))
               for(i in seq_along(tempname)){
                 df1[[tempname[i]]]=NULL
               }
               df1
               df1=rbind(df1,temp)
          }
     }
     df1$no=1:nrow(df1)
     df1$stats=NULL
     df1
     fit
     df2=autoReg(fit,keepstats=TRUE,...)
      # df2= autoReg(fit,keepstats=TRUE,uni=TRUE)

     df2=df2%>% dplyr::filter(.data$id!="(Intercept)")

     df2
     df=dplyr::full_join(df1,df2,by="id")
     df
     df$mode[is.na(df$stats)]="Reference"
     df$stats[is.na(df$stats)]="Reference"

     if(max(nchar(df$desc),na.rm=TRUE)<1) widths[2]=0

     if(mode==1) {
          df$Estimate[is.na(df$Estimate)]=0
          xintercept=0
     } else if(mode==2) {
           df$OR[is.na(df$OR)]=1
           xintercept=1
     } else if(mode==3) {
             df$HR[is.na(df$HR)]=1
             xintercept=1
     }
     if(!show.ref) df=shorten(df)
     dodge=length(setdiff(unique(df$mode),c(NA,"Reference")))>1
     dodge

     limits=setdiff(unique(df$mode),"Reference")
     p <-ggplot(df,aes_string(x=xname))+
          geom_vline(xintercept=xintercept,color="grey30",lty=2)

     if(dodge){
          p=p+   geom_errorbar(aes(y=reorder(.data$id,.data$no),xmin=.data$lower,xmax=.data$upper,color=mode),
                               position=position_dodge(width=0.4),width=0.1)
          if(change.pointsize){
               p=p+geom_point(aes(y=reorder(.data$id,.data$no),fill=mode,size=.data$n),
                              position=position_dodge(width=0.4),pch=22)
          } else {
               p=p+geom_point(aes(y=reorder(.data$id,.data$no),fill=mode),
                              position=position_dodge(width=0.4),pch=22)
          }

     } else{
          p=p+   geom_errorbar(aes(y=reorder(.data$id,.data$no),xmin=.data$lower,xmax=.data$upper),width=0.1)
     if(change.pointsize){
             p=p+geom_point(aes(y=reorder(.data$id,.data$no),size=.data$n),pch=15)
     } else {
             p=p+geom_point(aes(y=reorder(.data$id,.data$no)),pch=15)
     }

     }
     p
     p= p+  labs(y="",x=xlabel)+
          scale_y_discrete(limits=rev)
     if(bw) {
      p=p+ scale_color_grey(start=0,end=0.2)+
           scale_fill_grey()
     }
     p=p+  guides(size="none")+
          theme_bw()+
          theme(axis.title.x = element_text(), axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(), legend.position=ifelse(dodge,"top","none"))




     tab_base <- ggplot(df,aes(y=reorder(.data$id,.data$no))) +
          scale_y_discrete(limits=rev)+
          ylab(NULL) + xlab(" ") +
          theme(plot.title = element_text(hjust = 0.5, size=12), ## centering title on text
                axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
                axis.line=element_blank(),
                axis.text.y=element_blank(),axis.ticks=element_blank(),
                axis.title.y=element_blank(),legend.position="none",
                panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),plot.background=element_blank())

     tab1 <- tab_base +
          geom_text(aes(x=1,label=.data$name)) +
          ggtitle("")
     tab1
     tab2 <- tab_base +
          geom_text(aes(x=1, label=.data$desc)) +
          ggtitle("")
     tab2
     if(dodge){
     tab3<-tab_base+
          geom_text(aes(x=1, label=.data$stats,group=mode),position=position_dodge(width=0.4))+
          guides(color="none")
     } else{
          tab3<-tab_base+
               geom_text(aes(x=1, label=.data$stats))
     }


     result=list(
       tab1=tab1,
       tab2=tab2,
       tab3=tab3,
       p=p,
       widths=widths,
       legend.position=legend.position)

     class(result)="modelPlot"

     result

}

#' S3 method for an class modelPlot
#' @param x An object of class modelPlot
#' @param ... Further arguments to be passed to plot()
#' @importFrom patchwork plot_layout
#' @export
print.modelPlot=function(x,...){

  print(x$tab1+x$tab2+x$tab3+x$p+plot_layout(ncol=4,widths=x$widths,guides="collect") &
    theme(legend.position=x$legend.position))
}
