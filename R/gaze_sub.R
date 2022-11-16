#'Summarize numeric vector to statistical summary
#' @param x A numeric vector
#' @param  digits integer indicating the number of decimal places
#' @param method An integer indicating methods for continuous variables. Possible values in methods are
#' 1 forces analysis as normal-distributed
#' 2 forces analysis as continuous non-normal
#' 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal
#' Default value is 1.
#' @param p A numeric
#' @examples
#' library(moonBook)
#' num2stat(acs$age)
#' num2stat(acs$age,method=2)
#' @return A character vector of length 1
#' @export
num2stat=function(x,digits=1,method=1,p=NULL){
     plusminus="\u00b1"
     fmt=paste0("%.",digits,"f")
     if(method==3){
          if(is.null(p)) {
               method=1
          } else{
               method=ifelse(p<0.05,2,1)
          }
     }
     if(method==1){
        result=paste0(sprintf(fmt,mean(x,na.rm=TRUE))," ",plusminus," ",
            sprintf(fmt,sd(x,na.rm=TRUE)))
     } else{
          result=paste0(sprintf(fmt,fivenum(x,na.rm=TRUE)[3])," (",
                 sprintf(fmt,fivenum(x,na.rm=TRUE)[2])," to ",
                 sprintf(fmt,fivenum(x,na.rm=TRUE)[4]),")")
     }
     result
}

#'Make description for numeric summary
#'@param method numeric
#'@param p A numeric or NULL
#'@return A character vector of length 1
descNum=function(method=1,p=NULL){
     plusminus="\u00b1"
     if(method==3){
          if(is.null(p)) {
               method=1
          } else{
               method=ifelse(p<0.05,2,1)
          }
     }
     if(method==1){
          paste0("Mean ",plusminus," SD")
     } else{
          "Median (IQR)"
     }
}

#' Summary function for continuous variable
#' @param data A data.frame
#' @param x A name of variable
#' @param y A name of variable, either continuous or categorical
#' @param max.ylev max.ylev An integer indicating the maximum number of levels of grouping variable ('y').
#'  If a column have unique values less than max.ylev it is treated as a categorical variable. Default value is 5.
#' @param digits integer indicating the number of decimal places
#' @param show.total logical. Whether or not show total column
#' @param show.n logical. Whether or not show N column
#' @param show.missing logical. Whether or not show missing column
#' @param show.stats logical. Whether or not show stats column
#' @param show.p logical. Whether or not show p column
#' @param method method	An integer indicating methods for continuous variables. Possible values in methods are
#' 1 forces analysis as normal-distributed
#' 2 forces analysis as continuous non-normal
#' 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal
#' Default value is 1.
#' @param origData A data.frame containing original data
#' @param ... Further arguments
#' @importFrom stats addmargins fivenum sd
#' @return An object of class "data.frame" or "tibble"
#' @examples
#' gazeCont(mtcars,"hp")
#' gazeCont(mtcars,"hp","mpg")
#' require(moonBook)
#' gazeCont(acs,"log(age)")
#' gazeCont(acs,"age",method=2)
#' gazeCont(acs,"age","EF",method=2)
#' gazeCont(acs,"age","Dx",method=1)
#' gazeCont(acs,"age","Dx",show.p=TRUE,method=3)
#' @export
gazeCont=function(data,x,y=NULL,max.ylev=5,digits=1,show.total=FALSE,show.n=FALSE,show.missing=FALSE,show.stats=TRUE,show.p=TRUE,method=1,origData,...){

        # data=acs;x="log(age)";y=NULL;max.ylev=5;digits=2;show.total=FALSE;show.p=TRUE;method=3;show.n=TRUE;show.missing=TRUE

     plusminus="\u00b1"
     fmt=paste0("%.",digits,"f")
     xname=x
     xname1=x
     conversion=FALSE
     if(!(xname %in% names(data)) & str_detect(xname,"\\(")){
             temp1=unlist(strsplit(xname,"\\("))
             xname=temp1[2]
             xname=str_replace(xname,"\\)","")
             xname=str_replace(xname,"\\^[0-9]*","")
             conversion=TRUE
     }
     x= data[[xname]]
     if(is.null(y)){
          temp=descNum(method)
          if(conversion){
                 y=paste0(temp1[1],"(x)")
                 y
                 x=eval(parse(text=y))
                 x
                 stats=num2stat(x,digits=digits,method=method)
          } else{
             stats=num2stat(x,digits=digits,method=method)
          }
          df=data.frame(name=xname1,desc=temp,stats=stats)
          if(show.n) df$n=sum(!is.na(x))
     } else{
          y= data[[y]]
          if(is.numeric(y) & (length(unique(y))>max.ylev)){
             name=xname
             desc=paste0("[",round(min(x,na.rm=TRUE),digits),",",
                         round(max(x,na.rm=TRUE),digits),"]")
             unit=descNum(method)
             value=num2stat(x,digits=digits,method=method)
             df=data.frame(name=xname,desc,unit,value)
             df
          } else{
               myp=my.t.test2(y,x,method=method,all=TRUE)[1]
               myp
          df=data.frame(x,y)
          df %>%
               group_by(.data$y) %>%
               summarise(
                    stats=num2stat(x,digits=digits,method=method,p=myp)
               ) %>% pivot_wider(names_from=.data$y,values_from=.data$stats) %>%
               mutate(name=xname,desc=descNum(method,p=myp)) %>%
               select(name,desc,everything())->df
          df
             if(show.total) {
                  df %>% mutate(
                       total=num2stat(x,digits=digits,method=method,p=myp)
                  ) ->df
             }
             df
             df1=data.frame(addmargins(table(y),1))
             levels(df1$y)[nrow(df1)]="total"
             df1$name=paste0(df1$y," (N=",df1$Freq,")")
             names(df)[3:ncol(df)]=df1$name[1:(ncol(df)-2)]
             df
             if(show.p){
                  df$p=my.t.test2(y,x,method=method,all=FALSE)%>% p2character2(add.p=FALSE)
             }
          }

     }
     df$N=as.character(sum(!is.na(x)))
     df$Missing=paste0(sum(is.na(x)),"(",sprintf(fmt,sum(is.na(x))*100/length(x)),"%)")
     df$type="continuous"
     df$id=df$name
     df<- df %>% select(.data$name,.data$desc,.data$N,.data$Missing,everything())
     if(show.n==FALSE) df$N=NULL
     if(show.missing==FALSE) df$Missing=NULL
     if(show.stats==FALSE) df$stats=NULL
     df
}

#' Summary function for categorical variable
#' @param data A data frame
#' @param x  Name of a categorical variable
#' @param y Name of a variable, either continuous or categorical
#' @param max.ylev max.ylev An integer indicating the maximum number of levels of grouping variable ('y').
#'  If a column have unique values less than max.ylev it is treated as a categorical variable. Default value is 5.
#' @param digits Numeric
#' @param show.total logical. Whether or not show total column
#' @param show.n logical. Whether or not show N column
#' @param show.missing logical. Whether or not show missing column
#' @param show.stats logical. Whether or not show stats column
#' @param show.p logical. Whether or not show p column
#' @param method method	An integer indicating methods for continuous variables. Possible values in methods are
#' 1 forces analysis as normal-distributed
#' 2 forces analysis as continuous non-normal
#' 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal
#' Default value is 1.
#' @param catMethod An integer indicating methods for categorical variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{0}{Perform chisq.test first. If warning present, perform fisher test}
#'                  \item{1}{Perform chisq.test without continuity correction}
#'                  \item{2}{Perform chisq.test with continuity correction}
#'                  \item{3}{perform fisher.test}
#'                  \item{4}{perform prop.trend test}
#'               }
#'               Default value is 2.
#' @param origData A data.frame containing original data
#' @param maxCatLevel An integer indicating the maximum number of unique levels of categorical variable.
#' If a column have unique values more than maxCatLevel, categorical summarization will not be performed.
#' @param ... Further arguments
#' @return An object of class "data.frame" or "tibble"
#' @examples
#' require(moonBook)
#' gazeCat(acs,"Dx")
#' gazeCat(acs,"Dx","smoking")
#' gazeCat(acs,"sex","Dx",show.p=TRUE)
#' gazeCat(acs,"Dx","sex",show.p=TRUE)
#' gazeCat(acs,"Dx","EF")
#' gazeCat(acs,"sex","EF",method=2)
#' gazeCat(mtcars,"cyl","hp")
#' @importFrom dplyr select group_by rename summarise mutate bind_cols count `%>%`
#' @importFrom stringr str_pad
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr everything
#' @export
gazeCat=function(data,x,y=NULL,max.ylev=5,digits=1,show.total=FALSE,show.n=FALSE,show.missing=FALSE,show.stats=TRUE,origData=NULL,show.p=TRUE,method=1,catMethod=2,maxCatLevel=20,...){

     # data=acs[acs$Dx=="Unstable Angina",];x="Dx";y="sex";
        # data=iris;x="Species";y="Sepal.Length"
         # data=mtcars;x="test";y=NULL
         # max.ylev=5;digits=2;origData=NULL;show.total=FALSE;show.p=TRUE;catMethod=2
         # show.n=FALSE;show.missing=FALSE;show.stats=TRUE;origData=NULL;show.p=FALSE;method=1;catMethod=2
     #maxCatLevel=20
     #  origData=acs
     # cat("nrow(data)=",nrow(data),"\n")
     # cat("nrow(origData)=",nrow(origData),"\n")
     xname=x
     x= data[[x]]
     plusminus="\u00b1"
     fmt=paste0("%.",digits,"f")
     if(is.null(y)){
             if(length(unique(x))>maxCatLevel){
                     name=xname
                     desc=paste0(length(unique(x))," unique values" )
                     stats=""
                     id=xname
                     res=data.frame(name=name,desc=desc,stats=stats,id=id,stringsAsFactors = FALSE)
                     if(show.n) res$n=length(x)
             } else if(sum(is.na(x))==length(x)){
                  name=xname
                  desc="all missing"
                  stats=""
                  id=xname
                  res=data.frame(name=name,desc=desc,stats=stats,id=id,stringsAsFactors = FALSE)
                  if(show.n) res$n=length(x)
             }else{
     res=as.data.frame(table(x))
          res %>%
          mutate(
          ratio=paste0(" (",sprintf(fmt,.data$Freq*100/sum(.data$Freq)),"%)"),
          stats=paste0(.data$Freq,.data$ratio),
          name=""
          ) %>%
          select(.data$name,.data$x,.data$stats) %>%
          rename(desc=.data$x) ->res
     res$name[1]=xname
     res$id=paste0(xname,res$desc)
     if(show.n) {
             res1=as.data.frame(table(x))
             res$n=res1$Freq
     }
             }
     } else{
          yname=y
          y= data[[y]]
          if(is.numeric(y) & (length(unique(y))>max.ylev)){
               df=data.frame(x,y)
               df %>%
                    group_by(.data$x) %>%
                    summarise(
                         value=num2stat(.data$y,digits=digits,method=method),
                         n=n()
                    )  %>%
                    mutate(name="",unit=descNum(method=method)) %>%
                    #mutate(x=paste0(str_pad(x,max(nchar(x)),"right"),"(N=",n,")")) %>%
                    select(.data$name,.data$x,.data$unit,.data$value) %>%
                    rename(desc=.data$x) ->df
               df$name[1]=xname
               df
               res=df
               res$id=paste0(xname,res$desc)
               res
               if(is.factor(x)) x=as.character(x)
               df1<-data.frame(x) %>% count(x) %>%
                 mutate(N=paste0(str_pad(x,maxnchar(x),"right")," (N=",n,")"))
               df1
               res$desc=df1$N
               res

          } else{
          result=table(x,y)

          if(nrow(result)==1){
              z=factor(origData[[xname]])
              levels(z)
              x=factor(x,levels=levels(z))
              result=table(x,y)
          }
          ratio=apply(result,2,function(x) {x*100/sum(x)})
          for(i in 1:nrow(result)){
               for(j in 1:ncol(result)){
                    result[i,j]=paste0(result[i,j]," (",round(ratio[i,j],1),"%)")
               }
          }
          result
          data.frame(result) %>%
               pivot_wider(names_from=.data$y,values_from = .data$Freq) %>%
               mutate(name="")%>%
               rename(desc=.data$x) -> res
          res$name[1]=xname
          res %>%select(.data$name,.data$desc,everything()) ->res
          res
          if(show.total){
               df=data.frame(x) %>% count(x)  %>%
                    mutate(ratio=round(.data$n*100/sum(.data$n),1),
                           total=paste0(.data$n," (",.data$ratio,"%)")
                           )
               res=bind_cols(res,total=df$total)
          }
          df1=data.frame(addmargins(table(y),1))
          levels(df1$y)[nrow(df1)]="total"
          df1$name=paste0(df1$y," (N=",df1$Freq,")")
          names(res)[3:ncol(res)]=df1$name[1:(ncol(res)-2)]
          res$id=paste0(xname,res$desc)
          res
          if(show.p){
               res$p=""
               res$p[1]=my.chisq.test2(x,y,catMethod=catMethod) %>% p2character2(add.p=FALSE)
          }
          }

     }
     res$type="categorical"

     res$desc=as.character(res$desc)
     df<-res
     df$N=""
     df$N[1]=sum(!is.na(x))
     df$Missing=""
     df$Missing[1]=paste0(sum(is.na(x)),"(",sprintf(fmt,sum(is.na(x))*100/length(x)),"%)")
     df<- df %>% select(.data$name,.data$desc,.data$N,.data$Missing,everything())
     if(show.n==FALSE) df$N=NULL
     if(show.missing==FALSE) df$Missing=NULL
     if(show.stats==FALSE) df$stats=NULL
     df
}


#' Summary function for categorical/continuous variable
#' @param data A data.frame
#' @param xname A name of categorical/continuous vector
#' @param y A name of vector, either continuous or categorical
#' @param max.ylev max.ylev An integer indicating the maximum number of levels of grouping variable ('y').
#'  If a column have unique values less than max.ylev it is treated as a categorical variable. Default value is 5.
#' @param autoCat logical Whether or not use is.mynumeric() to determine whether a variable is numeric or not
#' @param ... Further arguments to be passed to gazeCont() or gazeCat()
#' @importFrom stringr str_replace
#' @examples
#' require(moonBook)
#' gaze_sub(acs,"age")
#' gaze_sub(acs,"log(age)")
#' gaze_sub(acs,"I(age^2)")
#' gaze_sub(acs,"sex")
#' gaze_sub(acs,"age","EF")
#' gaze_sub(acs,"sex","EF")
#' gaze_sub(acs,"age","Dx")
#' gaze_sub(acs,"sex","Dx")
#' gaze_sub(iris,"Species","Sepal.Length")
#' gaze_sub(mtcars,"am")
#' gaze_sub(mtcars,"am",autoCat=TRUE)
#' @return An object of class "data.frame" or "tibble"
#' @export
gaze_sub=function(data,xname,y=NULL,max.ylev=5,autoCat=FALSE,...){

         # data=acs;xname="I(age^2)";y=NULL;max.ylev=5
     x=xname
     if(!is.null(y)) {
          if(identical(x,y)) return(NULL)
     }
     if(!(x %in% names(data)) & str_detect(x,"\\(")){
        x=unlist(strsplit(x,"\\("))[2]
        x=str_replace(x,"\\)","")
        x=str_replace(x,"\\^[0-9]*","")
     }
     myx=data[[x]]
     if(autoCat){
     if(is.mynumeric(myx,maxy.lev=max.ylev)) {
          df=gazeCont(data,xname,y,max.ylev,...)
              # gazeCont(data,xname,y,max.ylev)
     } else{
          df=gazeCat(data,xname,y,max.ylev,...)
     }
     } else{
          if(is.numeric(myx)) {
               df=gazeCont(data,xname,y,max.ylev,...)
               # gazeCont(data,xname,y,max.ylev)
          } else{
               df=gazeCat(data,xname,y,max.ylev,...)
          }
     }
     if(!is.null(attr(data[[xname]],"label"))){
          df$name[df$name==xname]=attr(data[[xname]],"label")
     }
     df

}



#' Statistical test for continuous variables
#' @param y a categorical vector
#' @param x a numeric vector
#' @param method method	An integer indicating methods for continuous variables. Possible values in methods are
#' 1 forces analysis as normal-distributed
#' 2 forces analysis as continuous non-normal
#' 3 performs a Shapiro-Wilk test or nortest::ad.test to decide between normal or non-normal
#' Default value is 1.
#' @param all A logical
#' @importFrom stats lm shapiro.test resid var.test t.test kruskal.test anova wilcox.test
#' @return A numeric vector of length 1
#' @examples
#' library(moonBook)
#' y=acs$sex
#' x=acs$height
#' my.t.test2(y,x)
#' @export
my.t.test2=function(y,x,method=1,all=FALSE){

     result=table(y,x)
     dim(result)

     (xlev=dim(result)[1])
     (ylev=dim(result)[1])
     if(ylev==1) {
          p=c(NA,NA,NA)
     } else if(xlev==2) {
          #browser()
          out=lm(x~y)
          if(sum(result)<=5000) {
               if(length(unique(resid(out)))==1){
                    out3=1
               } else{
                    out3=shapiro.test(resid(out))
               }

          } else {
               out3=nortest::ad.test(resid(out))
          }
          out1=try(var.test(x~y))

          if(!inherits(out1,"htest")) {
               p=c(NA,NA,NA)
          } else{
               suppressWarnings(out5<-wilcox.test(x~y))
               if(is.nan(out1$p.value)) {
                    p=c(NA,NA,out5$p.value)
               } else if(out1$p.value<0.05) {
                    out4=t.test(x~y,na.rm=T)
                    p=c(out3$p.value,out4$p.value,out5$p.value)
               } else {
                    out4=t.test(x~y,var.equal=TRUE)
                    p=c(out3$p.value,out4$p.value,out5$p.value)
               }

          }

     } else{
          out3=lm(x~factor(y))
          if(sum(result)<=5000) out4=shapiro.test(resid(out3))
          else out4=nortest::ad.test(resid(out3))
          out5=kruskal.test(as.numeric(x),factor(y))
          p=c(out4$p.value,anova(out3)$Pr[1],out5$p.value)
     }
     if(method==1){
          result=p[2]
     } else if(method==2){
          result=p[3]
     } else{
          result=ifelse(p[1]<0.05,p[3],p[2])
     }
     ifelse(all,p,result)
}

#' Statistical test for categorical variables
#' Statistical test for categorical variables
#' @param x a vector
#' @param y a vector
#' @param catMethod An integer indicating methods for categorical variables.
#'               Possible values in methods are
#'               \describe{
#'                  \item{0}{Perform chisq.test first. If warning present, perform fisher test}
#'                  \item{1}{Perform chisq.test without continuity correction}
#'                  \item{2}{Perform chisq.test with continuity correction}
#'                  \item{3}{perform fisher.test}
#'                  \item{4}{perform prop.trend test}
#'               }
#'               Default value is 2.
#' @param all A logical
#' @importFrom stats chisq.test fisher.test xtabs prop.trend.test
#' @importFrom moonBook cat.test
#' @return A numeric vector of length 1
#' @examples
#' library(moonBook)
#' x=acs$sex
#' y=acs$Dx
#' my.chisq.test2(x,y)
#' @export
my.chisq.test2=function(x,y,catMethod=2,all=FALSE)
{
     temp=table(y,x)
     temp
     if((nrow(temp)>2)&(ncol(temp)==2)) temp=t(temp)
     # temp=xtabs(~x+y)
     temp
     if(dim(temp)[2]==1){
          p=c(NA,NA,NA)
          attr(p,"method")=""
          # } else if(dim(temp)[1]==1){
          #     p=c(NA,NA,NA)
          #     attr(p,"method")=""
     } else{
          p=c(NA,NA,NA)
          if(catMethod==0) {
                suppressWarnings(result<-cat.test(temp))
          } else if(catMethod==1) {
                suppressWarnings(result<-chisq.test(temp,correct=FALSE))
          } else if(catMethod==2) {
                  suppressWarnings(result<-chisq.test(temp))
          } else if(catMethod==3) {
                  suppressWarnings(result<-cat.test(temp,mode=2))
          } else if(catMethod==4) {
               if(nrow(temp)>2) {
                    result=NA
               } else {
                  suppressWarnings(result<-prop.trend.test(temp[2,],colSums(temp)))
               }
          }

          if(length(result)==1){
               p[1]<-NA
               attr(p,"method")=""
          } else{
               p[1]=result$p.value
               attr(p,"method")=result$method
          }
          if(sum(temp)< 100 & dim(temp)[1]>1){
               suppressWarnings(p[2]<-fisher.test(temp)$p.value)
          }
          if(nrow(temp)!=2) {
               p[3]=NA
          } else {
               suppressWarnings(p[3]<-prop.trend.test(temp[2,],colSums(temp))$p.value)
          }

     }
     if(is.nan(p[1])) p[1]=1

     ifelse(all,p,p[1])
}
