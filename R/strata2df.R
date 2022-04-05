#'Convert a character vector to a data.frame
#'@param strata A character vector
#'@return A data.frame
#'@export
strata2df=function(strata){
     temp=strsplit(strata,", ")
     count=length(temp[[1]])
     temp1=data.frame(matrix(unlist(temp),ncol=count,byrow=TRUE))
     colnames(temp1)=gsub("=.*","",temp1[1,])
     temp2=data.frame(lapply(temp1,function(x) gsub(".*=","",x)))
     temp2
}

