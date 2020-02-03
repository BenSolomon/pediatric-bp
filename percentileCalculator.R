library(dplyr)

percentileCalculator <- function(age, height, sex, SBP.DBP){
  x <- height
  y <- age
  w <- (y-10)*(x-150)
  
  df <- paste(SBP.DBP, sex, "coef.csv", sep="_")
  df <- read.csv(df, row.names = 1)
  
  source(paste0("splineKnots_",sex,".R"))
  
  if (x-t1m < 0) {x2a=0} else {x2a=x-t1m}
  if (x-t4m < 0) {x2b=0} else {x2b=x-t4m}
  if (x-t5m < 0) {x2c=0} else {x2c=x-t5m}
  x2=x2a^3-x2b^3*(t5m-t1m)/(t5m-t4m)+x2c^3*(t4m-t1m)/(t5m-t4m)
  if (x-t2m < 0) {x3a=0} else {x3a=x-t2m}
  x3=x3a^3-x2b^3*(t5m-t2m)/(t5m-t4m)+x2c^3*(t4m-t2m)/(t5m-t4m)
  if (x-t3m < 0) {x4a=0} else {x4a=x-t3m}
  x4=x4a^3-x2b^3*(t5m-t3m)/(t5m-t4m)+x2c^3*(t4m-t3m)/(t5m-t4m)
  x2s=x2/100
  x3s=x3/100
  x4s=x4/100
  
  if (y-ta1m < 0 ) { y2a=0}  else {y2a=y-ta1m}
  if (y-ta4m < 0 ) { y2b=0}  else {y2b=y-ta4m}
  if (y-ta5m < 0 ) { y2c=0}  else {y2c=y-ta5m}
  y2=y2a^3-y2b^3*(ta5m-ta1m)/(ta5m-ta4m)+y2c^3*(ta4m-ta1m)/(ta5m-ta4m)
  if (y-ta2m < 0 ) { y3a=0}  else {y3a=y-ta2m}
  y3=y3a^3-y2b^3*(ta5m-ta2m)/(ta5m-ta4m)+y2c^3*(ta4m-ta2m)/(ta5m-ta4m)
  if (y-ta3m < 0 ) { y4a=0}  else {y4a=y-ta3m}
  y4=y4a^3-y2b^3*(ta5m-ta3m)/(ta5m-ta4m)+y2c^3*(ta4m-ta3m)/(ta5m-ta4m)
  y2s=y2/100
  y3s=y3/100
  y4s=y4/100
  
  if (w-tb1m < 0 ) { w2a=0}  else {w2a=w-tb1m}
  if (w-tb4m < 0 ) { w2b=0}  else {w2b=w-tb4m}
  if (w-tb5m < 0 ) { w2c=0}  else {w2c=w-tb5m}
  w2=w2a^3-w2b^3*(tb5m-tb1m)/(tb5m-tb4m)+w2c^3*(tb4m-tb1m)/(tb5m-tb4m)
  if (w-tb2m < 0 ) { w3a=0}  else {w3a=w-tb2m}
  w3=w3a^3-w2b^3*(tb5m-tb2m)/(tb5m-tb4m)+w2c^3*(tb4m-tb2m)/(tb5m-tb4m)
  if (w-tb3m < 0 ) { w4a=0}  else {w4a=w-tb3m}
  w4=w4a^3-w2b^3*(tb5m-tb3m)/(tb5m-tb4m)+w2c^3*(tb4m-tb3m)/(tb5m-tb4m)
  w2s=w2/100^2
  w3s=w3/100^2
  w4s=w4/100^2
  
  spline.position <- c("1"=1, "x"=x, "x2s"=x2s, "x3s"=x3s, "x4s"=x4s, 
                       "y"=y, "y2s"=y2s, "y3s"=y3s, "y4s"=y4s, 
                       "w"=w, "w2s"=w2s, "w3s"=w3s, "w4s"=w4s)
  df <- df %>%
    select(b0sys,
           b1sys,b2sys,b3sys,b4sys,
           ba1sys,ba2sys,ba3sys,ba4sys,
           bb1sys,bb2sys,bb3sys,bb4sys)
  df <- data.frame(t(apply(df, 1, function(x) x*spline.position)))
  df <- data.frame(percentile = 1:99/100, fxsys = apply(df, 1, sum))
  return(df)
}

