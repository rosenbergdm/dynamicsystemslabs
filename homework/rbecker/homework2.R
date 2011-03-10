       ##  R Assignment 2
       ##  Russell Becker

  # 1 (a)
# Fixed points:
# N = 41 * N - 10 * N^2
# 0 = 40 * N - 10 * N^2 = N * (40 - 10 * N)
# fp: N = 0, N = 4
# Stability:
# f'(N) = 41 - 20(N)
# f'(0) = 41 - 20(0) = 41 -> fp at N = 0 is unstable
# f'(4) = 41 - 20(4) = -39 -> fp at N = 4 is unstable


  # 1 (b)
# Fixed points:
# N = 41 * N + 2 * N^2
# 0 = 40 * N + 2 * N^2 = N * (40 + 2 * N)
# fp: N = 0, N = -20
# Stability:
# f'(N) = 41 + 4(N)
# f'(0) = 41 + 4(0) = 41 -> fp at N = 0 is unstable
# f'(-20) = 41 + 4(-20) = -39 -> fp at N = -20 is unstable


  # 2 (a)
imapfunction <- function (x) {
  return (x * (5 - 4 * x))
  }
maxiter <- 50
yvector <- xvector <- numeric (maxiter + 2)
yvector [1] <- 0
xvector [1] <- 0.1
for (ii in 1:50) {
  yvector [2 * ii] <- imapfunction (xvector [2 * ii - 1])
  yvector [2 * ii + 1] <- yvector [2 * ii]
  xvector [2 * ii] <- xvector [2 * ii - 1]
  xvector [2 * ii + 1] <- yvector [2 * ii + 1]
  }
plot (-10:50 / 10, imapfunction (-10:50 / 10), xlab = 'x',
  ylab = 'y', main = 'f(x) = x(5-4x) cobweb', type = 'l',
  xlim = c(0,4), ylim = c(0,2.5), xaxs = 'i', yaxs = 'i'
  )
lines (-2:6, -2:6, col = 'red')
lines (xvector, yvector, col = 'blue')
# graphically, the fixed points are at x = 0 and x = 1
# neither appears to be stable

  # 2 (b)
imapfunction <- function (x) {
  return (2 * x * (1 - 3 * x / 2))
  }
maxiter <- 50
yvector <- xvector <- numeric (maxiter + 2)
yvector [1] <- 0
xvector [1] <- 0.01
for (ii in 1:50) {
  yvector [2 * ii] <- imapfunction (xvector [2 * ii - 1])
  yvector [2 * ii + 1] <- yvector [2 * ii]
  xvector [2 * ii] <- xvector [2 * ii - 1]
  xvector [2 * ii + 1] <- yvector [2 * ii + 1]
  }
plot (-10:50 / 10, imapfunction (-10:50 / 10), xlab = 'x',
  ylab = 'y', main = 'f(x) = 2x(1-3x/2) cobweb', type = 'l',
  xlim = c(0,1), ylim = c(0,0.5), xaxs = 'i', yaxs = 'i'
  )
lines (-2:6, -2:6, col = 'red')
lines (xvector, yvector, col = 'blue')
# graphically, the fixed points are x = 0 and x = 1/3
# x = 0 appears to be unstable, while x = 1/3 appears to be stable


  # 2 (c)
imapfunction <- function (x) {
  return (x / 2 - x^2 / 5)
  }
maxiter <- 50
yvector <- xvector <- numeric (maxiter + 2)
yvector [1] <- 0
xvector [1] <- 1
for (ii in 1:50) {
  yvector [2 * ii] <- imapfunction (xvector [2 * ii - 1])
  yvector [2 * ii + 1] <- yvector [2 * ii]
  xvector [2 * ii] <- xvector [2 * ii - 1]
  xvector [2 * ii + 1] <- yvector [2 * ii + 1]
  }
plot (-10:50 / 10, imapfunction (-10:50 / 10), xlab = 'x',
  ylab = 'y', main = 'f(x) = x/2 - x^2/5 cobweb', type = 'l',
  xlim = c(0,4), ylim = c(0,2.5), xaxs = 'i', yaxs = 'i'
  )
lines (-2:6, -2:6, col = 'red')
lines (xvector, yvector, col = 'blue')
# graphically, the only fixed point is at x = 0
# this fixed point appears to be stable


  # 3
imapfunction <- function (x) {
  return ("user input manipulation of x")
  }
x0 <- "initial x value"
cobwebplot <- function (x0) {
  maxiter <- 50
  yvector <- xvector <- numeric (maxiter + 2)
  yvector [1] <- 0
  xvector [1] <- x0
  for (ii in 1:50) {
    yvector [2 * ii] <- imapfunction (xvector [2 * ii - 1])
    yvector [2 * ii + 1] <- yvector [2 * ii]
    xvector [2 * ii] <- xvector [2 * ii - 1]
    xvector [2 * ii + 1] <- yvector [2 * ii + 1]
    }
  plot (-10:50 / 10, imapfunction (-10:50 / 10), xlab = 'x',
    ylab = 'y', main = 'f(x) = x/2 - x^2/5 cobweb', type = 'l',
    xlim = c(0,4), ylim = c(0,2.5), xaxs = 'i', yaxs = 'i'
    )
  lines (-2:6, -2:6, col = 'red')
  lines (xvector, yvector, col = 'blue')
  }


  # 4 (a)
dG/dt = -kG
This is a linear, autonomous, homogenous ODE


  # 4 (b)
1/G * dG = -k * dt
ln(|G|) = -kt + C
e ^ ln(|G|) = e ^ (-kt + C)
G = e ^ (-kt + C)              e^C = A
G(0) = G0 = e ^ (-k(0) + C)
G0 = e^C
ln(G0) = C
solution:
G(t) = e ^ (-kt + ln(G0))


  # 4 (c)
timevector <- 0:360
xvector <- c(100)
for (ii in 2:361) {
  xvector [ii] <- (2.71828 ^ (-0.01 * timevector [ii] + 4.60517))
  }
plot (timevector, xvector, main = 'glucose level decline',
  xlab = 'time (min)', ylab = '[Glucose], mg/dL', type = 'l')


  # 4 (d)
dG/dt = (-0.01)G = 0 only when G = 0
The only fixed point in this model is at G =0
f'(G) = -0.01 for all G, so the fixed point is stable


  # 5 (a)
dG/dt = -kG + a           a(t) = -k, b(t) = a
This is a linear, autonomous, inhomogeneous ODE


  # 5 (b)
G(t) = e^(integral(-k)dt) * integral(a * e^(-integral(-k)dt))dt
 + C * e^(integral(-k)dt)
G(t) = e^(-kt) * integral(a * e^(kt))dt + C * e^(-kt)
G(t) = e^(-kt) * (a / k) * e^(kt) + C * e^(-kt)
G(t) = (a / k) + C * e^(-kt)
G(0) = G0 = (a/k) + C
C = G0 - (a/k)
G(t) = (a / k) + (G0 - (a/k)) * e^(-kt)


  # 5 (c)
G0 <- 100
k <- 0.01
a <- 4
timevector <- 0:600
xvector <- c(G0)
for (ii in 2:601) {
  xvector [ii]  <- ((a / k) + (G0 - (a/k)) * 2.71828^(-k * timevector [ii]))
  }
plot (timevector, xvector, main = 'glucose level equilibrium',
  xlab = 'time (min)', ylab = '[Glucose], mg/dL', type = 'l')


  # 5 (d)
The equilibrium for these parameters is at G = 400.  It is stable.
