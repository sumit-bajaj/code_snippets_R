# do.call constructs and executes a function call from a name or a function 
# and a list of arguments to be passed to it.



f <- function(a = 1, b = 2) {
  print(a)
  print(b)
}


args = list(1,"dd")

# execute the function call with list of arguments
do.call(f, args)

