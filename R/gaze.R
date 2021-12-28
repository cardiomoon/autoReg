#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#' @param x An R object, formula or data.frame
#' @param ... arguments to be passed to gaze.data.frame or gaze.formula
#' @importFrom magrittr `%>%`
#' @export
#' @examples
#' library(moonBook)
#' library(magrittr)
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
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#'
#' @param x An object of class "formula". Left side of ~ must contain the
#'                name of one grouping variable or two grouping variables in an
#'                additive way(e.g. sex+group~), and the right side of ~ must have
#'                variables in an additive way.
#'@param data A data.frame
#'@param keepid logical Whether or not keep id column
#'@param missing logical If true, missing value analysis performed
#'@param ... Further arguments to be passed to gaze()
#'@importFrom dplyr group_split
#'@importFrom purrr map_dfc map_dfr map2_dfc walk
#'@importFrom stats terms
#'@export
gaze.formula_sub=function(x,data,keepid=FALSE,missing=FALSE,...){

      #x=sex+Dx~.;data=acs;keepid=FALSE
     #x=sex~.;data=acs;keepid=FALSE
      # x=sex+Dx+DM~HBP
      # data=acs
     # #cat("gaze.formula_sub\n")
     #x=~hp*wt+am;data=mtcars;keepid=FALSE
     #x=sumY~Base+Age+Trt;data=breslow.dat

     f=x

     myt=terms(f,data=data)
     xvars=attr(myt,"term.labels")

     temp=strsplit(deparse(x),"~")[[1]][1]
     temp=gsub(" ","",temp)
     yvars=strsplit(temp,"+",fixed=TRUE)[[1]]
     yvars
     xvars
     # cat("yvars=",yvars,"\n")
     # cat("xvars=",xvars,"\n")
     if(length(yvars)==0){
         df=map_dfr(xvars, function(x){gaze_sub(data,x,origData=data,...)})%>%select(-.data$type)
         if(!keepid) df = select(df,-.data$id)
     } else if(length(yvars)==1){

       if(missing==TRUE){
         if(sum(is.na(data[[yvars]]))>0){
           data[[paste0(yvars,"Missing")]]=ifelse(is.na(data[[yvars]]),"Missing","Not missing")
           data[[paste0(yvars,"Missing")]]=factor(data[[paste0(yvars,"Missing")]],levels=c("Not missing","Missing"))
           s=paste0(paste0(yvars,"Missing"),"~",paste0(xvars,collapse="+"))
           data[[yvars]]<-NULL

           result=gaze(as.formula(s),data,keepid=keepid,missing=FALSE,...)
           attr(result,"missing")=TRUE
           return(result)
         } else{
           cat(paste0("There is no missing data in column '",yvars,"'\n"))
           s=paste0("~",paste0(xvars,collapse="+"))
           result=gaze(as.formula(s),data,keepid=keepid,missing=FALSE,show.n=TRUE,...)
           return(result)
         }
       }
         df=map_dfr(xvars, function(x){gaze_sub(data,x,yvars,origData=data,...)}) %>%select(-.data$type)
         if(!keepid) df = select(df,-.data$id)
         attr(df,"groups")=getGroupNames(data,yvars[1])


     } else {

         dflist <- data %>% group_by_at(yvars[-length(yvars)]) %>% group_split()

         df=map2_dfc(dflist,1:length(dflist),function(df,i){
              result=map_dfr(xvars,function(x){gaze_sub(df,x,yvars[length(yvars)],origData=data,...)}) %>% select(-.data$type)
              if(!keepid) result = select(result,-.data$id)
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
         attr(df,"groups")=getGroupNames(data,yvars[-length(yvars)])

     }
     attr(df,"yvars")=yvars
     class(df)=c("gaze","data.frame","tibble")
     df
}

#' Get sepcific group name
#' @param data A data.frame
#' @param yvars variable names
#' @examples
#' library(moonBook)
#' getGroupNames(acs,"sex")
#' getGroupNames(acs,c("sex","Dx"))
#' @importFrom dplyr group_by_at n
#' @export
getGroupNames=function(data,yvars){

     data %>%
          group_by_at(yvars) %>%
          summarise(n=n(),.groups="drop") %>%
          mutate(n=paste0("(N=",.data$n,")")) -> df
     df

}

#' Convert data.frame into flextable
#' @param x A data.frame
#' @param vanilla logical
#' @param  fontsize Numeric
#' @param digits integer indicating the position of decimal place
#' @param ... Further arguments to be passed to df2flextable()
#' @importFrom rrtable df2flextable
#' @importFrom flextable align autofit hline hline_top
#' @importFrom officer fp_border
#' @importFrom purrr map_chr
#' @examples
#' data(acs,package="moonBook")
#' library(magrittr)
#' gaze(acs) %>% myft()
#' gaze(sex~.,acs) %>% myft()
#' gaze(sex+Dx~.,data=acs,show.p=TRUE,show.total=TRUE,show.n=TRUE,shiw.missing=TRUE) %>% myft()
#' gaze(Dx+sex~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' gaze(Dx+sex+HBP~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' @export
myft=function(x,vanilla=TRUE,fontsize=10,digits,...){

     if("imputedReg" %in% class(x)){
        if(missing(digits)) digits=c(1,4,4,4,2,4,4,4,4,4,4,1)
     } else{
       names(x)[2]="levels"
       if(missing(digits)) digits=2
     }
     yvars=attr(x,"yvars")
     yvars
     if(length(yvars)>0){
       if(is.null(attr(x,"missing"))) {
         names(x)[1]=paste0("Dependent:",yvars[length(yvars)])
       } else{
         yname=str_remove(attr(x,"yvars"),"Missing")
         names(x)[1]=paste0("Dependent:",yname)
       }

     }
     vanilla=TRUE
     ft<-x %>% rrtable::df2flextable(vanilla=vanilla,fontsize=fontsize,digits=digits,...)


     if(length(yvars)>1){
          small_border = fp_border(color="gray", width = 1)
          df=attr(x,"groups")
          if(length(yvars)==2) {
               collapse=" "
          } else {
               collapse="\n"
          }
          groupvar=paste0(c(yvars[-length(yvars)],"(N)"),collapse=collapse)
          header=c(groupvar,map_chr(1:nrow(df),function(i){paste0(df[i,],collapse=collapse)}))
          length=nrow(df)
          no=(ncol(x)-2)%/%length
          widths=c(2,rep(no,length))

          ft<-ft %>%
               flextable::add_header_row(values=header,colwidths=widths) %>%
               hline(i=1,border=fp_border(color="gray", width = 0),part="header") %>%
               hline(i=1,border=fp_border(color="gray", width = 1),part="header") %>%
               hline_top(border=fp_border(color="black", width = 2),part="header")


     }

     ft %>%
       flextable::align(align="center",part="header") %>%
       flextable::align(j=1:2,align="left",part="body") %>%
          flextable::autofit()
}



