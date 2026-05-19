# Another file you often see in the wild is R/utils.R.
# This is a common place to define small utilities that are used inside multiple package functions.
# Since they serve as helpers to multiple functions, placing them in R/utils.R
# makes them easier to re-discover when you return to your package after a long break.

## probably won't need but just in case.

if_else = function(test, yes, no) {
  if (test) {
    return(yes)
  }
  else {
    return(no)
  }
}
