#' Restore fit model data containing AsIs expressions
#' @param fit An object of class lm, glm or coxph
#' @return a data.frame
#' @examples
#' require(survival)
#' pbc$status2=ifelse(pbc$status==2,1,0)
#' fit=coxph(Surv(time,status2)~age+log(bili),data=pbc)
#' fit2model(fit)
#' @importFrom stringr str_remove_all
#' @export
fit2model=function(fit){

     if("coxph" %in% class(fit)){
          dataname = as.character(fit$call)[3]
          data=eval(parse(text=dataname))
          f = fit$formula
          y = as.character(f)[2]
          temp1=str_remove_all(y,"Surv\\(|\\)| ")
          temp1=unlist(strsplit(temp1,","))
          timevar=temp1[1]
          statusvar=temp1[2]
          xvars = attr(fit$terms, "term.labels")
          xvars
          timevar

          if(str_detect(statusvar,"==")) {
               statusvar=unlist(strsplit(statusvar,"=="))[1]
          } else if(str_detect(statusvar,"!=")) {
                  statusvar=unlist(strsplit(statusvar,"!="))[1]

          }
          add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
          if(length(add)>0){
               xvars=setdiff(xvars,add)
               add=str_remove_all(add,"strata\\(|cluster\\(|frailty\\(|\\)")
               xvars=c(xvars,add)
          }
          myformula=paste0(timevar,"~",paste0(c(statusvar,xvars),collapse="+"))
          myformula
          fit0=lm(myformula,data=data)
          modelData=fit0$model
     } else if("glmerMod" %in% class(fit)){
          modelData=fit@frame
          data=modelData
     } else if("glm" %in% class(fit)){

          y = as.character(fit$formula)[2]
          y

          if(str_detect(y,"==")) {
               dataname = as.character(fit$call)[3]
               data=eval(parse(text=dataname))
               f = fit$formula
               y = as.character(f)[2]
               y
               xvars = attr(fit$terms, "term.labels")
               xvars
               if(str_detect(y,"==")) {
                    temp=unlist(strsplit(y,"=="))[1]
                    temp=str_replace_all(temp," ","")
                    xvars=c(xvars,temp)
               } else if(str_detect(y,"!=")) {
                       temp=unlist(strsplit(y,"!="))[1]
                       temp=str_replace_all(temp," ","")
                       xvars=c(xvars,temp)
               }
               add=xvars[str_detect(xvars,"strata\\(|cluster\\(|frailty\\(")]
               if(length(add)>0){
                    xvars=setdiff(xvars,add)
                    add=str_remove_all(add,"strata\\(|cluster\\(|frailty\\(|\\)")
                    xvars=c(xvars,add)
               }
               myformula=paste0(y,"~",paste0(xvars,collapse="+"))
               myformula
               fit0=lm(myformula,data=data)
               modelData=fit0$model
          } else{
               modelData=fit$model
          }
          data=modelData
     } else{
          dataname = as.character(fit$call)[3]
          modelData=eval(parse(text=dataname))

     }
     modelData %>%
          restoreData() %>%
          restoreData2() %>%
          restoreData3() -> df
     df


}

#' restore data with factor in column name
#' @param data data.frame
restoreData=function (data)
{
     select = which(str_detect(names(data), "\\(.*\\)"))
     select
     for (i in seq_along(select)) {
          str_detect(names(data)[select[i]], "factor")
          if (str_detect(names(data)[select[i]], "factor")) {
               temp = as.numeric(as.character(data[[select[i]]]))
               tempname = str_replace(names(data)[select[i]], ".*\\(",
                                      "")
               tempname = str_replace(tempname, "\\)", "")
               data[[tempname]] = temp
          }
     }
     if (length(select) > 0)
          data[-select]
     data
}

#' Whether a string vector can be converted to numeric
#' @param x A string vector
beNumeric=function (x)
{
     str_replace_all(x, "([:digit:]|\\.|\\/).*", "") == ""
}


#' restore data with I() in column name
#' @param df data.frame
#' @importFrom stringr str_extract
restoreData2=function (df)
{
     seek = which(str_detect(names(df), "I\\("))
     for (i in seq_along(seek)) {
          x = names(df)[seek[i]]
          x = str_replace_all(x, "^I\\(|\\)$", "")
          if (str_detect(x, "\\^")) {
               operator = "^"
               res = unlist(strsplit(x, "\\^"))
               res = str_replace_all(res, "\\(|\\)", "")
          }
          else if (str_detect(x, "\\*")) {
               operator = "*"
               res = unlist(strsplit(x, "\\*"))
               res = str_replace_all(res, "\\(|\\)", "")
          }
          varname = res[!beNumeric(res)]
          number = res[beNumeric(res)]
          if (is.null(df[[varname]])) {
               temp = paste0("df[[", seek[i], "]]", operator, "(1/(",
                             number, "))")
               df[[varname]] = eval(parse(text = temp))
          }
     }
     seek = which(str_detect(names(df), "^log[0-9]*\\("))
     for (i in seq_along(seek)) {
          x = names(df)[seek[i]]
          res = unlist(strsplit(x, "\\("))
          number = str_extract(res[1], "[0-9]*$")
          x = str_replace_all(x, "^log[0-9]*\\(|\\)$", "")
          varname = x
          varname
          number
          if (is.null(df[[varname]])) {
               if (number == "") {
                    temp = paste0("exp(df[[", seek[i], "]])")
               }
               else {
                    temp = paste0(number, "^(df[[", seek[i], "]])")
               }
               df[[varname]] = eval(parse(text = temp))
          }
     }
     seek = which(str_detect(names(df), "^exp\\("))
     for (i in seq_along(seek)) {
          x = names(df)[seek[i]]
          varname = str_replace_all(x, "^exp\\(|\\)$", "")
          if (is.null(df[[varname]])) {
               temp = paste0("log(df[[", seek[i], "]])")
               df[[varname]] = eval(parse(text = temp))
          }
     }
     seek = which(str_detect(names(df), "^sqrt\\("))
     for (i in seq_along(seek)) {
          x = names(df)[seek[i]]
          varname = str_replace_all(x, "^sqrt\\(|\\)$", "")
          if (is.null(df[[varname]])) {
               temp = paste0("(df[[", seek[i], "]])^2")
               df[[varname]] = eval(parse(text = temp))
          }
     }
     df
}

#'restore data with operator in column name
#'@param df a data.frame
#'@param changeLabel logical
restoreData3=function (df, changeLabel = FALSE)
{
     pattern = "/|-|\\+|\\*|\\^"
     select1 = which(str_detect(names(df), pattern))
     select2 = which(str_detect(names(df), "I\\("))
     select = setdiff(select1, select2)
     addgroup = FALSE
     if (changeLabel) {
          changelabel = TRUE
          if (str_detect(df$label[1], "\\|")) {
               addgroup = TRUE
               df$label3 = str_extract(df$label, "\\|.*")
          }
          df$label = str_replace(df$label, "\\|.*", "")
     }
     for (i in seq_along(select)) {
          tempname = names(df)[select[i]]
          temp = str_replace_all(tempname, " ", "")
          temp
          operator = str_extract(temp, pattern)
          operator
          temp1 = unlist(strsplit(temp, pattern))
          temp1
          if (sum(beNumeric(temp1)) != 1)
               next
          number = temp1[beNumeric(temp1)]
          varname = temp1[!beNumeric(temp1)]
          pos = which(beNumeric(temp1))
          number
          varname
          pos
          if (pos == 1) {
               if (operator %in% c("/", "-")) {
                    eq = paste0(number, operator, "df[['", tempname,
                                "']]")
                    if (changeLabel)
                         df$label = paste0(number, operator, "(", df$label,
                                           ")")
               }
               else if (operator == "^") {
                    eq = paste0("log(df[['", tempname, "']])/log(",
                                number, ")")
                    if (changeLabel)
                         df$label = paste0("log(", df$label, ")/log(",
                                           number, ")")
               }
               else {
                    operator = revOperator(operator)
                    eq = paste0("df[['", tempname, "']]", operator,
                                number)
                    if (changeLabel)
                         df$label = paste0("(", df$label, ")", operator,
                                           number)
               }
          }
          else {
               if (operator == "^") {
                    eq = paste0("df[['", tempname, "']]^(1/", number,
                                ")")
                    if (changeLabel)
                         df$label = paste0("(", df$label, ")", "^(1/",
                                           number, ")")
               }
               else {
                    operator = revOperator(operator)
                    eq = paste0("df[['", tempname, "']]", operator,
                                number)
                    if (changeLabel)
                         df$label = paste0("(", df$label, ")", operator,
                                           number)
               }
          }
          eq
          df
          df[[varname]] = eval(parse(text = eq))
     }
     if (changeLabel) {
          if (addgroup)
               df$label = paste0(df$label, df$label3)
     }
     df
}

#'get opposite arithmetic operator
#'
#'get opposite arithmetic operator
#'@param operator A character
revOperator=function (operator)
{
     result = operator
     if (operator == "+")
          result = "-"
     else if (operator == "-")
          result = "+"
     else if (operator == "*")
          result = "/"
     else if (operator == "/")
          result = "*"
     result
}
