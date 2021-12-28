#'Retur maximum character number except NA
#'@param x a vector
#'@examples
#'x=c(1,2,"sadf",NA)
#'maxnchar(x)
#'data(acs,package="moonBook")
#'lapply(acs,maxnchar)
#'@export
maxnchar=function(x){
    x[is.na(x)]=""
    max(nchar(x))
}

#' S3 method print for an object of class gaze
#' @param x An object of class gaze
#' @param ... Further arguments
#' @importFrom purrr map_chr pmap_dfc map_int pmap_chr
#' @importFrom stringr str_split str_trim
#' @examples
#' data(acs,package="moonBook")
#' x=gaze(acs,show.n=TRUE,show.missing=TRUE)
#' gaze(sex~.,acs,show.p=TRUE,show.n=TRUE,show.missing=TRUE,show.total=TRUE)
#' gaze(Dx+sex~.,acs,show.p=TRUE)
#' gaze(sex+Dx+HBP~.,acs,show.p=TRUE)
#' @export
print.gaze=function(x,...){

     # x1=map_dfc(x,function(y){
     #      if(is.numeric(y)) {
     #           y=sprintf("%.3f",y)
     #      }
     #      y
     # })
     # attr(x1, "yvars")=attr(x, "yvars")
     # x=x1
     x[is.na(x)]=""
     names(x)[2]="levels"
     yvars=attr(x,"yvars")
     yvars
     mode=1
     if(length(yvars)>0){
          names(x)[1]=paste0("Dependent:",yvars[length(yvars)])
          if(ncol(x)>4) mode=mode+length(yvars)
     }

     temp=str_split(names(x),fixed("("),simplify=TRUE)
     title1=str_trim(temp[,1],"both")
     groupno=nrow(attr(x,"groups"))
     lengths1=map_int(names(x),maxnchar)
     lengths2=map_int(x,maxnchar)
     lengths=pmax(lengths1,lengths2)+2
     lengths
     mode
     if(mode>2){
          drawline(sum(lengths));cat("\n")
          groups=attr(x,"groups")
          names(groups)[ncol(groups)]="(N)"
          groupno=nrow(groups)
          groups
          each=(length(lengths)-2)/groupno
          headerlengths=lengths[1]+lengths[2]
          start=3
          for(j in 1:groupno){
               headerlengths=c(headerlengths,sum(lengths[start:(start+each-1)]))
               start=start+each
          }
          side=rep("both",groupno+1)
          headerlengths
          if(mode==3){
               groups
               groups$header=paste(groups[[1]],groups[[2]])
               header=c(paste0(names(groups)[1:2],collapse=" "),groups$header)
               header
               headerlengths
               side
               list(header,headerlengths,side) %>% pmap_chr(str_pad) -> header
               cat(paste0(header,collapse=""),"\n")
          } else{
               for(i in 1:ncol(groups)){
                    header=c(names(groups)[i],groups[[i]])
                    list(header,headerlengths,side) %>% pmap_chr(str_pad) -> header
                    cat(paste0(header,collapse=""),"\n")
               }
          }
     }
     no=ncol(x)

     side=rep("both",no)
     list(title1,lengths,side) %>% pmap_chr(str_pad) -> title1
     if(mode>1) {
     for(i in seq_along(temp[,2])){
          if(temp[,2][i]!="") temp[,2][i]=paste0("(",temp[,2][i])
     }
     title2=temp[,2]
     title2[1]="(N)"
     list(title2,lengths,side) %>% pmap_chr(str_pad) -> title2
     }
     side=c("right","right",rep("left",no-2))
     list(x,lengths,side) %>% pmap_dfc(str_pad) ->x1
     drawline(sum(lengths));cat("\n")
     cat(paste0(title1,collapse=""));cat("\n")
     if(mode>1) {
          cat(paste0(title2,collapse=""));cat("\n")
     }
     drawline(sum(lengths));cat("\n")

     for(i in 1:nrow(x1)){
          cat(paste0(x1[i,],collapse=""),"\n")
     }
     drawline(sum(lengths));cat("\n")
}


#' draw line character
#' @param n Numeric
#' @examples
#' drawline(10)
#' @export
drawline=function(n){
     x=paste0(rep("\u2014",n),collapse="")
     cat(x)
}

