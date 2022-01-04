#' Find duplcated term
#' @param x A vector
#' @importFrom dplyr lag
#' @examples
#' x=rep(1:5,each=3)
#' findDup(x)
#' which(!findDup(x))
#'@export
findDup=function(x){
     if(length(x)==0) return(NULL)
     else if(length(x)==2) return(TRUE)
     y=dplyr::lag(x)
     result=c()
     for(i in 1:length(x)){
          if(is.na(y[i])) {
               result=c(result,FALSE)
          }else if(y[i]==x[i]) {
               result=c(result,TRUE)
          } else{
               result=c(result,FALSE)
          }
     }
     result
}


#' Find first duplicated position
#' @param x a vector
#' @importFrom dplyr lead
#' @examples
#'x=rep(1:5,each=3)
#' which(find1stDup(x))
#'@export
find1stDup=function(x){
     if(length(x)==0) return(NULL)
     y=findDup(x)
     z=dplyr::lag(y)
     dplyr::lead(y & (!z))
}


#' Remove duplcated term
#' @param x A vector
#' @param replacement A character to be replaced or NA
#' @importFrom dplyr lag
#' @examples
#' x=rep(1:5,each=3)
#' removeDup(x)
#'@export
removeDup=function(x,replacement=""){
     pos=findDup(x)
     x[pos]=replacement
     x
}

#'filldown vector with lead value
#'@param x a vector
#'@param what Values to be filled
#'@examples
#'x=rep(1:5,each=3)
#'x=removeDup(x,NA)
#'filldown(x)
#'@export
filldown=function(x,what=c("",NA)){
     temp=x[1]
     for(i in 2:length(x)){
          if(x[i] %in% what){
               x[i]=temp
          } else{
               temp=x[i]
          }
     }
     x
}

#'Shorten an object of class gaze
#'@param x an object of class gaze
#'@param xname A variable name
#'@param ref Numeric Th number to be used as reference
#'@examples
#'data(acs,package="moonBook")
#'x=gaze(sex~.,data=acs)
#'shorten(x)
#'@export
shorten=function(x,xname=NULL,ref=1){
     if(is.null(xname)) xname=names(x)[1]
     x[[xname]]=filldown(x[[xname]])
     pos=which(find1stDup(x[[xname]]))
     if(ref==2) pos=pos+1
     result=c()
     for(i in seq_along(pos)){
        if(x[[2]][pos[i]]!="") result=c(result,pos[i])
     }
     x=x[-result,]
     x[[xname]]=removeDup(x[[xname]])
     x
}



