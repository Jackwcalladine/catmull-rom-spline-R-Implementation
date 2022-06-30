# Implementation of catmull rom splines

a = c(2,1)
b = c(4,2)
c = c(1,3)
d = c(6,4)


crom_spline <- function(p0,p1,p2,p3,tau, alpha) {
  
  structure <- list(
    a = c(0,0),
    b = c(0,0),
    c = c(0,0),
    d = c(0,0)
  )
  
  t0 = 0
  t01 = t0 + `^`(dist(rbind(p0, p1)), alpha)
  t12 = t01 + `^`(dist(rbind(p1, p2)), alpha)
  t23 = t12 + `^`(dist(rbind(p2, p3)), alpha)
  
  m1 = (1 - tau) * (p2 - p1 + t12 * ((p1 - p0) / t01 - (p2 - p0) / (t01 + t12)))
  m2 = (1 - tau) * (p2 - p1 + t12 * ((p3 - p2) / t23 - (p3 - p1) / (t12 + t23)))
  
  structure$a = 2 * (p1 - p2) + m1 + m2
  structure$b = -3 * (p1 - p2) - m1 - m1 - m2
  structure$c = m1
  structure$d = p1
  
  print(structure)
  
}

crom_spline(a,b,c,d,0,0.5)

xframe <- data.frame(x = numeric())
for (i in list(a,b,c,d)) {
  xframe <- rbind(xframe, i[1])
  print(xframe)
  
}

yframe <- data.frame(y = numeric())
for (i in list(a,b,c,d)) {
  yframe <- rbind(xframe, i[1])
  print(yframe)
}



