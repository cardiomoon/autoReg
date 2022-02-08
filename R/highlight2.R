#' Highlight a data.frame
#' @param x A data.frame
#' @param i numeric rows to highlight
#' @param j numeric columns to hightlight
#' @param style A style function or NULL
#' @param include.colname logical Whether or not include colname
#' @importFrom crayon red bold
#' @return a data.frame
#' @export
#' @examples
#' head(mtcars) %>% highlight2(i=3) %>% printdf()
#' library(crayon)
#' head(mtcars) %>% highlight2(i=2) %>% highlight2(j=3,style=blue$bold) %>% printdf()
#' fit=lm(mpg~wt*hp,data=mtcars)
#' gaze(fit)
#' gaze(fit) %>% highlight2(j=4,include.colname=TRUE)
#' gaze(fit) %>% highlight2(i=2,j=4) %>% highlight2(i=2,j=2:3,style=blue$bold)
#' gaze(fit) %>% highlight2(i=2) %>% highlight2(j=3,style=blue$bold)
highlight2=function(x,i=NULL,j=NULL,style=NULL,include.colname=FALSE){
     # x=mtcars;i=2;j=NULL;style=NULL
if(is.null(style)){
   style  = red $ bold
}
if(!is.null(i)) {
     if(is.null(j)) j=1:ncol(x)
}
if(!is.null(j)) {
     if(include.colname) colnames(x)[j]=style(colnames(x)[j])
     if(is.null(i)) i=1:nrow(x)
}
rows=i
cols=j
for(i in seq_along(rows)){
     for(j in seq_along(cols)){
          x[rows[i],cols[j]]=style(x[rows[i],cols[j]])
     }
}
x
}
