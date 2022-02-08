#'Change p value to string
#'@param x a numeric
#'@param digits integer indicating decimal place
#'@param add.p logical
#'@return A character vector
#'@export
p2character2=function(x,digits=3,add.p=TRUE){
     if(is.numeric(x)){
     cut = 1/(10^digits)
     temp = sprintf(paste0("%.", digits, "f"), x)
     temp = stringr::str_replace(temp, "^0", "")
     temp
     for (i in 1:length(x)) {
          if (is.na(x[i])) {
               temp[i] = ""
          }
          else if (x[i] < cut) {
               temp[i] = stringr::str_replace(temp[i], "0$", "1")
               if(add.p){
                    temp[i] = paste0("p<", temp[i])
               } else{
                    temp[i] = paste0("<", temp[i])
               }
          } else{
               if(add.p) {
                    temp[i]=paste0("p=",temp[i])
               } else{
                    temp[i]
               }
          }
     }
     temp
     } else{
         temp=x
     }
     temp
}
