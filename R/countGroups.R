#' Count groups
#' @param data A data.frame
#' @param yvars variable names
#' @examples
#' library(moonBook)
#' countGroups(acs,"sex")
#' countGroups(acs,c("sex","Dx"))
#' @importFrom dplyr group_by_at n
#' @return An object of class "tibble"
#' @export
countGroups=function(data,yvars){

     data %>%
          group_by_at(yvars) %>%
          summarise(n=n(),.groups="drop") %>%
          mutate(n=paste0("(N=",.data$n,")")) -> df
     df

}
