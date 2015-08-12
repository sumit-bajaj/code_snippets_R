nosim = 20
n = 10

sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))


matrix(rnorm(nosim * n), nosim)


rnorm(nosim * n)

sd(apply(matrix(rnorm(nosim * n), nosim), 1, mean))

1/sqrt(n)

library(UsingR)

hist(rbinom(10,30000,.5))


# prob of getting upto 1 success in two trials
pbinom(1, 2, 0.5)

# prob of getting at least one success in two trials
pbinom(0, 2, 0.5, lower.tail = F)

# get more than 6 successes in 8 trials
pbinom(6, 8, 0.5, lower.tail = FALSE)

dnorm(seq(-3, 3, length = 10))

qnorm(.95, mean=0, sd=1) 
qnorm(.95)

pnorm(1160, mean=1020, sd=50, lower.tail=F) 

qnorm(.75, mean=1020, sd=50) 

ppois(30, lambda = 2.5 * 4)

x = binom.test(56, 100, conf.level = .90)
x$conf.int

pnorm(70, mean = 80, sd = 10)

qnorm(mean = 1100,sd =  56.25, p = .95)

pbinom(3, prob = 0.5, size = 5, lower.tail = F)

pnorm(1)


test = prop.test(x = 5, n = 1008, p = 0.3, conf.level = .99)

explain(test)

library(explainr)

t.test(mtcars$mpg)

explain(prop.test(x = 100, n = 1000, p = 0.1))

