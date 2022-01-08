#'Get interaction data from data
#'@param name a string with interaction term
#'@param data a data.frame
#'@importFrom stringr str_remove str_remove_all
#'@export
#'@return An object of class "data.frame"
#'@examples
#'data(acs,package="moonBook")
#'getInteraction("TC:Dx:sex",data=acs)
getInteraction=function(name,data){
     temp=unlist(strsplit(name,":"))
     no=length(temp)
     id=list()
     for(i in seq_along(temp)){
          if(!(temp[i] %in% names(data))){
               id[[i]]=temp[i]
          } else if(is.numeric(data[[temp[i]]])) {
               id[[i]]=temp[i]
          } else{
               id[[i]]=paste0(temp[i],sort(unique(data[[temp[i]]])))
          }
     }
     answer=c()
     name=c()
     desc=c()
     for(i in 2:no){
          res=combn(x=1:no,i)
          for(j in 1:ncol(res)){
               temp1=apply(expand.grid(id[res[,j]]),1,function(x){paste0(x,collapse=":")})
              answer=c(answer,temp1)
              name=c(name,rep(paste0(temp[res[,j]],collapse=":"),length(temp1)))
          }
     }
     desc=str_remove_all(answer,paste0(temp,collapse="|"))
     desc=str_remove(desc,"^:|:?")
     n=getN(name,desc,data=data)
     data.frame(name=name,desc=desc,id=answer,n=n)
}

#' Get number of data specified by 'name' and 'desc'
#' @param name a string with interaction term
#' @param desc character
#' @param data a data.frame
#' @export
#' @return A numeric vector
#' @examples
#' data(acs,package="moonBook")
#' df=getInteraction("TC:Dx:sex",data=acs)
#' getN(name=df$name,desc=df$desc,data=acs)
getN=function(name,desc,data){

     result=c()
     for(j in seq_along(name)){

          temp=unlist(strsplit(name[j],":"))
          temp
          seq_along(temp)
          for(i in seq_along(temp)){
               if(!(temp[i] %in% names(data))){
                    temp=temp[-i]
               } else if(is.numeric(data[[temp[i]]])) {
                    temp=temp[-i]
               }
          }
          length(temp)

          if(length(temp)==0) {
               result=c(result,nrow(data))
          } else if(length(temp)==1) {
               result=c(result,nrow(data[data[[temp]]==desc[j],]))
          } else{
               desc1=unlist(strsplit(desc[j],":"))
               desc1
               temp
               data1=data
               for(i in 1:length(temp)){

                    data1=data1[data1[[temp[i]]]==desc1[i],]
               }
               result=c(result,nrow(data1))
          }
     }
     result
}

