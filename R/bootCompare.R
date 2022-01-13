#'Prepare data for analysis with dabestr and bootstrap-coupled estimation of effect size
#'
#'Prepare data for analysis with dabestr and bootstrap-coupled estimation of effect size
#'This function is a wrapper function for \code{\link[dabestr]{dabest}} and \code{\link[dabestr]{mean_diff}}.
#'You can select variables for \code{\link[dabestr]{dabest}} function using dplyr::select.
#'This function also compatible with wide-form data.
#'@param .data A data.frame or tibble
#'@param ... Column names in .data
#'@param idx A vector containing factors or strings in the grouping columns.
#'@param paired logical  If TRUE, the groups are treated as paired samples.
#'@param id.column Default NULL. A column name indicating the identity of the datapoint if the data is paired. This must be supplied if paired is TRUE.
#'@importFrom dabestr dabest mean_diff
#'@importFrom dplyr enexprs sym
#'@importFrom rlang .data `!!`
#'@importFrom tidyr pivot_longer
#'@export
#'@return A dabest_effsize object with 10 elements. Details are described in \code{\link[dabestr]{mean_diff}}.
#'@examples
#'require(dplyr)
#'require(dabestr)
#'bootCompare(iris,Species,Sepal.Length) %>% plot()
#'\donttest{
#'data(acs,package="moonBook")
#'acs %>% select(sex,age) %>% bootCompare() %>% plot()
#'data(Anorexia,package="PairedData")
#'bootCompare(Anorexia,Prior,Post)
#'bootCompare(Anorexia) %>% plot()
#'data(Blink2,package="PairedData")
#'Blink2 %>% dplyr::select(2:4) %>% bootCompare() %>% plot()
#'bootCompare(Blink2,Resting,Straight,Oscillating,paired=FALSE)
#'}
bootCompare=function(.data,...,idx,paired,id.column=NULL){

     myvars = enexprs(...)
     vars=as.character(unlist({{myvars}}))
     if(length(vars)==0) vars=names(.data)

     if(is.mynumeric(.data[[vars[1]]]) & is.mynumeric(.data[[vars[2]]])){
          mydata=.data[vars]
          mydata$id=1:nrow(mydata)
          longdf=mydata %>% pivot_longer(cols=all_of(vars))
          if(missing(paired)) paired=TRUE
          result<-dabest(longdf,!!sym("name"),!!sym("value"),idx=vars,paired=paired,id.column=!!sym("id")) %>% mean_diff()
          return(result)
     }
     if(is.numeric(.data[[vars[1]]]) &(!is.numeric(.data[[vars[[2]]]]))){
          temp=myvars[[1]]
          myvars[[1]]=myvars[[2]]
          myvars[[2]]=temp
          vars=as.character(unlist({{myvars}}))
     }
     if(missing(idx)){
          if(is.factor(.data[[vars[1]]])) {
               idx=levels(.data[[vars[1]]])
          } else{
               idx=sort(unique(.data[[vars[1]]]))
          }
     }
     mydata=na.omit(.data[vars])
     if(missing(paired)) paired=FALSE
     dabest(mydata,!!sym(vars[1]),!!sym(vars[2]),idx=idx,paired=paired,id.column=id.column) %>% mean_diff()
}
