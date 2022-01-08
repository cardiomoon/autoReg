This is a resubmission of the package 'autoReg'.

## Test environments
* local OS X install, R 4.1.2
* win-builder (devel and release)
* rhub

## R CMD check results
There were no ERRORs or WARNINGs.


Reply from CRAN on the last submission

1. Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)
Missing Rd-tags in up to 48 .Rd files, e.g.:
     addFitSummary.Rd: \value
     autoReg_sub.Rd: \value
     autoReg.Rd: \value
     autoRegCox.Rd: \value
     bootPredict.Rd: \value
     ...
-> I have add \value to all .Rd files. Thank you! 

2. You are setting options(warn=-1) in your function. This is not allowed.
To avoid unnecessary warning output you could use e.g. suppressWarnings().
-> my reply: I have removed options(warn=-1) in my function. Thank you !

Please fix and resubmit.
-> I haved fixed all you have commented. Thank you!
