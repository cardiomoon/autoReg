#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA, Kruskal-Wallis, chi-squared, Fisher's,...)
#' @param x An R object, formula or data.frame
#' @param ... arguments to be passed to gaze.data.frame or gaze.formula
#' @importFrom dplyr `%>%`
#' @export
#' @examples
#' library(moonBook)
#' library(dplyr)
#'gaze(acs)
#'gaze(~age+sex,data=acs)
#'gaze(sex~.,data=acs,digits=1,method=1,show.p=TRUE) %>% myft()
#'\donttest{
#'gaze(sex~age+Dx,data=acs)
#'gaze(EF~.,data=acs) %>% myft()
#'gaze(sex+Dx~.,data=acs,show.p=TRUE) %>% myft()
#'gaze(sex+Dx~.,data=acs)
#'gaze(Dx+sex~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#'gaze(Dx+sex+HBP~cardiogenicShock,data=acs,show.p=TRUE)
#'gaze(~mpg+cyl,data=mtcars)
#'gaze(~.,data=mtcars)
#'gaze(cyl~.,data=mtcars,show.p=TRUE)
#'gaze(hp~.,data=mtcars)
#'gaze(cyl+am~.,data=mtcars)
#'}
#' @return An object of class "gaze" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#'}
#'@export
gaze=function(x,...)  UseMethod("gaze")


#'@describeIn gaze S3 method for formula
#'@export
gaze.formula=function(x,...) {
     gaze.formula_sub(x,...)
}

#'@describeIn gaze default S3 method
#'@export
gaze.data.frame=function(x,...){
  gaze(~.,x,...)
}



#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA, Kruskal-Wallis, chi-squared, Fisher's,...)
#'
#' @param x An object of class "formula". Left side of ~ must contain the
#'                name of one grouping variable or two grouping variables in an
#'                additive way(e.g. sex+group~), and the right side of ~ must have
#'                variables in an additive way.
#'@param data A data.frame
#'@param missing logical If true, missing value analysis performed
#'@param ... Further arguments to be passed to gaze()
#'@importFrom dplyr group_split
#'@importFrom purrr map_dfc map_dfr map2_dfc walk
#'@importFrom stats terms
#' @return An object of class "gaze" which inherits from the class "data.frame"
#' with at least the following attributes:
#' \describe{
#' \item{attr(*,"yvars)}{character. name of dependent variable}
#'}
#'@export
gaze.formula_sub=function(x,data,missing=FALSE,...){

      #x=sex+Dx~.;data=acs;
     #x=sex~.;data=acs;
      # x=sex+Dx+DM~HBP
      # data=acs
     # #cat("gaze.formula_sub\n")
      # x=mpg~hp*wt+am+disp;data=mtcars;
     #x=sumY~Base+Age+Trt;data=breslow.dat
      # x=~Sepal.Length*Species;data=iris;missing=FALSE
         # x=~log(age)+sex;data=acs;missing=FALSE

     f=x

     myt=terms(f,data=data)
     xvars=attr(myt,"term.labels")
     del=str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")
     if(any(del)) xvars=xvars[-which(del)]
     others=c()
      others=setdiff(xvars,names(data))
      if(length(others)>0) xvars=setdiff(xvars,others)


     temp=strsplit(deparse(x),"~")[[1]][1]
     temp=gsub(" ","",temp)
     yvars=strsplit(temp,"+",fixed=TRUE)[[1]]
     yvars
     xvars
     names(data)=gsub(" ","",names(data))
     # cat("yvars=",yvars,"\n")
     # cat("xvars=",xvars,"\n")
     if(length(yvars)==0){
         df=map_dfr(xvars, function(x){gaze_sub(data,x,origData=data,...)})%>%select(-.data$type)
           # df=map_dfr(xvars, function(x){gaze_sub(data,x,origData=data)})%>%select(-.data$type)

     } else if(length(yvars)==1){

       if(missing==TRUE){
         if(sum(is.na(data[[yvars]]))>0){
           data[[paste0(yvars,"Missing")]]=ifelse(is.na(data[[yvars]]),"Missing","Not missing")
           data[[paste0(yvars,"Missing")]]=factor(data[[paste0(yvars,"Missing")]],levels=c("Not missing","Missing"))
           s=paste0(paste0(yvars,"Missing"),"~",paste0(xvars,collapse="+"))
           data[[yvars]]<-NULL

           result=gaze(as.formula(s),data,missing=FALSE,...)
           attr(result,"missing")=TRUE
           return(result)
         } else{
           cat(paste0("There is no missing data in column '",yvars,"'\n"))
           s=paste0("~",paste0(xvars,collapse="+"))
           result=gaze(as.formula(s),data,missing=FALSE,show.n=TRUE,...)
           return(result)
         }
       }
         df=map_dfr(xvars, function(x){gaze_sub(data,x,yvars,origData=data,...)}) %>%select(-.data$type)
         attr(df,"groups")=countGroups(data,yvars[1])


     } else {

         dflist <- data %>% group_by_at(yvars[-length(yvars)]) %>% group_split()
         df=dflist[[1]]
         i=1
         df=map2_dfc(dflist,1:length(dflist),function(df,i){

              result=map_dfr(xvars,function(x){gaze_sub(df,x,yvars[length(yvars)],origData=data,...)}) %>% select(-.data$id,-.data$type)

              if(i>1) {
                   result=result %>% select(-.data$name,-.data$desc)

                   what=c("p","N","Missing")
                   for(j in seq_along(what)){
                        x=what[j]
                        if(x %in% names(result)) {
                             names(result)[names(result)==x]=str_pad(x,width=nchar(x)+i,"right")
                        }
                   }

              }
              result
         })
         attr(df,"groups")=countGroups(data,yvars[-length(yvars)])

     }

     if(length(others)>0){

       for(i in 1:length(others)){
            # i=1
         name=others[i]
         desc="others"
         if(str_detect(name,":")) {
           desc="interaction"
           temp=getInteraction(name,data=data)
           temp$n=NULL

         } else if(str_detect(name,fixed("I("))){
           desc="interpretation"
           temp=data.frame(name=name,levels=desc,id=name)
         } else{
           desc="interpretation"
           temp=data.frame(name=name,levels=desc,id=name)
         }
         df
         temp$stats=""
         class(df)="data.frame"
         df=bind_rows(df,temp)
       }
     }
     attr(df,"yvars")=yvars
     class(df)=c("gaze","data.frame","tibble")
     df
}







