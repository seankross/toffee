compliment_link <- function(link) {
  if(link == "logit") {
    exp
  } else if(link == "identity") {
    function(x){x}
  } else if(link == "log") {
    function(x){10^x}
  } else if(link == "sqrt") {
    function(x){x^2}
  } else if(link == "inverse") {
    function(x){x^(-1)}
  } else {
    stop("Toffee: Cannot find compliment link function.")
  }
}
