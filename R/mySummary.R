#' Produce table for descriptive statistics
#'
#' Produce table for descriptive statistics by groups for several variables easily.
#' Depending on  the nature of these variables, different descriptive statistical
#' methods were used(t-test, ANOVA,Kruskal-Wallis, chisq, Fisher,...)
#' @param x An R object, formula or data.frame
#' @param ... arguments to be passed to mySummary.data.frame or mySummary.formula
#' @importFrom magrittr `%>%`
#' @export
#' @examples
#' library(moonBook)
#' library(magrittr)
#'mySummary(acs)
#'mySummary(~age+sex,data=acs)
#'mySummary(sex~.,data=acs,digits=1,method=1,show.p=TRUE) %>% myft()
#'mySummary(sex~.,data=acs)
#'mySummary(sex~age+Dx,data=acs)
#'mySummary(EF~.,data=acs) %>% myft()
#'mySummary(sex+Dx~.,data=acs,show.p=TRUE) %>% myft()
#'mySummary(sex+Dx~.,data=acs)
#'mySummary(Dx+sex~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#'mySummary(Dx+sex+HBP~cardiogenicShock,data=acs,show.p=TRUE)
#'mySummary(~mpg+cyl,data=mtcars)
#'mySummary(~.,data=mtcars)
#'mySummary(cyl~.,data=mtcars,show.p=TRUE)
#'mySummary(hp~.,data=mtcars)
#'mySummary(cyl+am~.,data=mtcars)
#'@export
mySummary=function(x,...)  UseMethod("mySummary")


#'@describeIn mySummary S3 method for formula
#'@export
mySummary.formula=function(x,...) {
     mySummary.formula_sub(x,...)
}

#'@describeIn mySummary default S3 method
#'@export
mySummary.data.frame=function(x,...){
  mySummary(~.,x,...)
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
#'@param ... Further arguments to be passed to mySummary()
#'@importFrom dplyr group_split
#'@importFrom purrr map_dfc map_dfr map2_dfc
#'@importFrom stats terms
#'@export
mySummary.formula_sub=function(x,data,keepid=FALSE,...){

      #x=sex+Dx~.;data=acs;keepid=FALSE

      # x=sex+Dx+DM~HBP
      # data=acs
     # #cat("mySummary.formula_sub\n")
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
         df=map_dfr(xvars, function(x){mySummary_sub(data,x,origData=data,...)})%>%select(-.data$type)
         if(!keepid) df = select(df,-.data$id)
     } else if(length(yvars)==1){
         df=map_dfr(xvars, function(x){mySummary_sub(data,x,yvars,origData=data,...)}) %>%select(-.data$type)
         if(!keepid) df = select(df,-.data$id)
         attr(df,"groups")=getGroupNames(data,yvars[1])


     } else {

         dflist <- data %>% group_by_at(yvars[-length(yvars)]) %>% group_split()

         df=map2_dfc(dflist,1:length(dflist),function(df,i){
              result=map_dfr(xvars,function(x){mySummary_sub(df,x,yvars[length(yvars)],origData=data,...)}) %>% select(-.data$type)
              if(!keepid) result = select(result,-.data$id)
              if(i>1) {
                   result=result %>% select(-.data$name,-.data$desc)
                   #if(!keepid) result = select(result,-.data$id)
                   if("p" %in% names(result)) {
                        names(result)[names(result)=="p"]=str_pad("p",width=i+1,"right")
                   }
              }
              result
         })
         attr(df,"groups")=getGroupNames(data,yvars[-length(yvars)])

     }
     attr(df,"yvars")=yvars
     class(df)=c("mySummary","data.frame","tibble")
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
#' @importFrom rrtable df2flextable
#' @importFrom flextable align autofit hline hline_top
#' @importFrom officer fp_border
#' @importFrom purrr map_chr
#' @examples
#' data(acs,package="moonBook")
#' library(magrittr)
#' mySummary(acs) %>% myft()
#' mySummary(sex~.,acs) %>% myft()
#' mySummary(sex+Dx~.,data=acs,show.p=TRUE) %>% myft()
#' mySummary(Dx+sex~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' mySummary(Dx+sex+HBP~cardiogenicShock,data=acs,show.p=TRUE) %>% myft()
#' @export
myft=function(x,vanilla=TRUE){

     names(x)[2]="levels"
     yvars=attr(x,"yvars")
     if(length(yvars)>0){
          names(x)[1]=paste0("Dependent:",yvars[length(yvars)])
     }
     vanilla=TRUE
     ft<-x %>% rrtable::df2flextable(vanilla=vanilla,fontsize=10)


     if(length(yvars)>1){
          small_border = fp_border(color="gray", width = 1)
          df=attr(x,"groups")
          groupvar=paste0(yvars[-length(yvars)],collapse="\n")
          header=c(groupvar,map_chr(1:nrow(df),function(i){paste0(df[i,],collapse="\n")}))
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



