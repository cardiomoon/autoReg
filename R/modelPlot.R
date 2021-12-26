#' Draw coefficients/odds ratio/hazard ratio plot
#' @param fit An object of calss glm
#' @param widths Numeric vector
#' @param change.pointsize logical Whether or not change point size
#' @importFrom ggplot2 ggplot geom_vline geom_point geom_errorbar labs scale_y_discrete
#' guides theme_bw theme element_text element_blank geom_text ggtitle aes xlab ylab aes_string
#' @importFrom patchwork plot_layout
#' @importFrom stats reorder
#' @importFrom dplyr full_join
#' @examples
#' fit=lm(mpg~wt*hp+am,data=mtcars)
#' modelPlot(fit,width=c(1,0,2,3))
#' fit=lm(Sepal.Width~Species*Sepal.Length,data=iris)
#' modelPlot(fit)
#' data(cancer,package="survival")
#' fit=glm(status~rx+age+sex+nodes+obstruct+perfor,data=colon,family="binomial")
#' modelPlot(fit)
#' data(colon_s,package="finalfit")
#' fit=glm(mort_5yr~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s,family="binomial")
#' modelPlot(fit)
#' library(survival)
#' fit=coxph(Surv(time,status)~age+sex+obstruct+perfor,data=colon)
#' modelPlot(fit)
#' fit=coxph(Surv(time,status)~age.factor+sex.factor+obstruct.factor+perfor.factor,data=colon_s)
#' modelPlot(fit)
#' @export
modelPlot=function(fit,widths=NULL,change.pointsize=TRUE){


     if(is.null(widths)) widths=c(1.2,1,2,3.5)

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
     if(mode==3){
         dataname = as.character(fit$call)[3]
         data=eval(parse(text=dataname))
         myformula=paste0("~",paste0(xvars,collapse="+"))
         df1=mySummary(as.formula(myformula),data=data,show.n=TRUE,keepid=TRUE)
     } else{
          data=fit$model[-1]
          data

          df1=mySummary(~.,data=data,show.n=TRUE,keepid=TRUE)
     }
     df1
     plusminus="\u00b1"
     df1$desc[df1$desc==paste0("Mean ",plusminus," SD")]=""
     others=setdiff(xvars,names(data))
     others
     if(length(others)>0){
          for(i in 1:length(others)){
                # i=1
               name=others[i]
               desc="others"
               if(str_detect(name,":")) {
                    temp=unlist(strsplit(name,":"))
                    factornames=c()
                    numericnames=c()
                    for(j in seq_along(temp)){
                        if(is.factor(data[[temp[j]]])|is.character(data[[temp[j]]])){
                             factornames=c(factornames,temp[[j]])
                        } else{
                             numericnames=c(numericnames,temp[[j]])
                        }
                    }
                    if(length(factornames)>0){
                         formula=paste0("~",paste0(factornames,collapse = "+"))
                         tempdf=as.data.frame(xtabs(as.formula(formula),data=data),stringsAsFactors = FALSE)
                         tempdf
                         n=tempdf$Freq
                         N=n
                         desc=apply(tempdf[-ncol(tempdf)],1,paste0,collapse=",")
                         tempdf2=tempdf
                         paste0(names(tempdf)[1],tempdf2[1,1])
                         for(i in 1:nrow(tempdf)){
                         for(j in 1:(ncol(tempdf)-1)){
                            tempdf[i,j]=paste0(names(tempdf)[j],tempdf2[i,j])
                            tempdf[1,1]=paste0(names(tempdf)[1],tempdf2[1,1])
                         }
                         }
                         id=apply(tempdf[-ncol(tempdf)],1,paste0,collapse=":")
                         id
                         if(length(numericnames)>0){
                           id=paste0(id,":",paste0(numericnames,collapse=":"))
                         }
                    } else{
                        id=name
                        n=nrow(data)
                        N=nrow(data)
                    }

               } else if(str_detect(name,fixed("I("))){
                    desc="interpretation"
                    id=name
                    n=nrow(data)
                    N=NA
               }
               temp=data.frame(name=name,desc=desc,N=N,stats="",n=n,id=id)
               df1=rbind(df1,temp)
          }
     }
     df1$no=1:nrow(df1)
     df1$stats=NULL
     df1
     fit

     df2=fit2stats(fit) %>% filter(.data$id!="(Intercept)")

     df2
     df=full_join(df1,df2,by="id")
     df$stats[is.na(df$stats)]="Reference"
     df
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
     p <-ggplot(df,aes_string(x=xname))+
          geom_vline(xintercept=xintercept,color="red",lty=2)
     if(change.pointsize){
             p=p+geom_point(aes(y=reorder(.data$id,.data$no),size=.data$n),pch=15)
     } else {
             p=p+geom_point(aes(y=reorder(.data$id,.data$no)),pch=15)
     }
     p=p+   geom_errorbar(aes(y=reorder(.data$id,.data$no),xmin=.data$lower,xmax=.data$upper),width=0.1)+
          labs(y="",x=xlabel)+
          scale_y_discrete(limits=rev)+
          guides(size="none")+
          theme_bw()+
          theme(axis.title.x = element_text(), axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(), legend.position = "none")

     p

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
     tab3<-tab_base+
          geom_text(aes(x=1, label=.data$stats))

     tab1+tab2+tab3+p+plot_layout(ncol=4,widths=widths)

}
