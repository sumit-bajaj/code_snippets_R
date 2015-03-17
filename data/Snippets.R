f <- function(a = 1, b = 2) {
  print(a)
  print(b)
}
f()
args = list(1,"dd")
do.call(f, args)

