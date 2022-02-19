#' Add labels to data
#'
#' @param data A data.frame
#' @return A data.frame
#' @export
#' @examples
#' addLabelData(data.frame(ph.ecog=0:3,sex=c(1,2,2,2),age=c(20,30,40,70)))
addLabelData=function(data){
     newdata=data
     tolabel=names(newdata)[which(sapply(newdata,function(x){length(unique(x))>1}))]
     df1=newdata %>% select(all_of(tolabel))
     suppressMessages(df2<-map2_dfc(names(df1),df1,function(x,y){
          paste0(x,"=",y)
     }))
     labels=apply(df2,1,paste0,collapse=",")
     attr(newdata,"labels")=labels
     newdata
}
