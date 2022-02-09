#' Draw an Observed vs Expected plot
#' @param fit An object of class "coxph"
#' @param xnames Character Names of explanatory variable to plot
#' @param no integer Number of groups to be made
#' @param maxy.lev Integer Maximum unique length of a numeric variable to be treated as categorical variables
#' @param median logical
#' @return  No return value, called for side effects
#' @importFrom dplyr semi_join as_tibble
#' @importFrom graphics legend lines title
#' @importFrom purrr map2_chr
#' @importFrom scales hue_pal
#' @importFrom moonBook rank2group
#' @export
#' @examples
#' library(survival)
#'data(cancer,package="survival")
#'fit=coxph(Surv(time,status)~rx,data=colon)
#'fit=coxph(Surv(time,status)~rx+age+strata(sex),data=colon)
#'OEplot(fit)
#'fit=coxph(Surv(time,status)~age,data=colon)
#'OEplot(fit)
#'fit=coxph(Surv(time,status)~logWBC,data=anderson)
#'OEplot(fit)
OEplot=function(fit,xnames=NULL,no=3,maxy.lev=5,median=TRUE){
     #     xname="grp";maxy.lev=5
                 # xnames=NULL;maxy.lev=5;median=TRUE;no=3
     data=fit2model(fit)
     xvars = attr(fit$terms, "term.labels")
     xvars
     if(is.null(xnames)) xnames=xvars[1]
     newdata=fit2newdata(fit,xnames=xnames,maxy.lev=maxy.lev,median=median)
     labels=attr(newdata,"labels")

     if((length(xnames)==1) &(is.mynumeric(data[[xnames]]))){
          data$temp=rank2group(data[[xnames]],no)
          if(no==3) {
               labs=c("Low","Medum","High")
          } else{
               labs=1:no
          }
          data$temp = factor(data$temp, levels = 1:no, labels = labs)
          data[[xnames]]=data$temp
           string=paste(unlist(strsplit(deparse(fit$terms),"~"))[1],"~",xnames)
           temp=paste0("coxph(",string,",data=data)")
           fit=eval(parse(text=temp))
           newdata=data.frame(labs)
           names(newdata)=xnames
           labels=labs
     }


     no=length(labels)
     col=scales::hue_pal()(no)
     fit1=survfit(fit,newdata=newdata)
     plot(fit1,lty=2,col=col,lwd=1,
          main=paste0("Observed versus Expected Plot by ",paste0(xnames,collapse=",")))
     legend("bottomleft",legend=labels,col=col,lwd=2)
     fit2=survfit(fit$terms,data=data)
     survdata=survfit2df(fit2)
     newdata[]=lapply(newdata,as.character)
     newdata1=as_tibble(newdata)
     newdata1
     add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
     add
     if(length(add)>0){
           temp=unlist(strsplit(gsub("\\)","",add),"\\("))
           temp
           temp1=paste0(temp[1],".",temp[2],".")
           names(newdata1)[names(newdata1)==temp[2]]=temp1
     }

     for(i in 1:nrow(newdata1)){
          df=semi_join(survdata,newdata1[i,],by=names(newdata1))
          lines(df$time,df$surv,col=col[i],type="s",lwd=2)
     }
     legend("topright",legend=c("Expected","Observed"),lty=c(2,1),lwd=c(1,2))
     df1=as_tibble(newdata %>% select(-all_of(xnames)))
     df1=df1[1,]
     label=map2_chr(names(df1),df1,function(x,y){
             paste0(x,"=",y)
     })
     label=paste0(label,collapse=", ")
     title(sub=label)


}





