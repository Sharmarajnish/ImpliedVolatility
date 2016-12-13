#' Calculate and plot implied volatility using Black Scholes model
#'
#' @param strike price
#' @param type
#' @param Option price
#' @param future price
#' @param time
#' @return calcualted sigma and plots
#' @export

plotImpliedVol <- function(strike=c(50,20),
                           type=c('C','P'),
                           optionPrice=c(1.62,0.01),
                           futurePrice=c(48.03,48.03),
                           time_to_expiry=c(0.1423,0.1423)) {
  library(quantmod)
  df<-data.frame(strike=strike,
                 type=type,
                 optionPrice=optionPrice,
                 futurePrice=futurePrice,
                 time_to_expiry=time_to_expiry)
  df$type <- as.character(df$type)

  #getting interest rate from US treasury. Last column of the retrieved data is the recent interest rate
  getSymbols('^IRX',src="yahoo",from=Sys.Date()-10,to=Sys.Date())
  intfree_rate= as.numeric(IRX[nrow(IRX),ncol(IRX)])
  r <- 4*log(1/(1-(intfree_rate/100)*(91/360))) #13-weeks continuously compounding rate.

  BS_Price <- function(type,S,K,T,r,SD) {
    d1 <- (log(S/K)+0.5*SD^2*T)/(SD*sqrt(T))
    d2 <- d1 - SD*sqrt(T)
    OptionPrice <- ifelse(type=='C',
                          exp(-r*T)*(S*pnorm(d1) - K*pnorm(d2)),
                          exp(-r*T)*(-S*pnorm(-d1) + K*pnorm(-d2)))
    return(OptionPrice)
  }

  impliedVol <- function(type,S,K,T,r,selling_price){
    SD_init <- 0.5
    SD_max <- 5
    SD_min <- 0
    iteration <- 0
    eta <- BS_Price(type,S,K,T,r,SD_init) - selling_price

    while (abs(eta)>=0.000001 && count < 2000) {
      if (eta<0){
        SD_min <- SD_init
        SD_init <- (SD_max+SD_init)/2
      } else {
        SD_max <- SD_init
        SD_init <- (SD_min+SD_init)/2
      }
      eta <- BS_Price(type,F,K,T,r,SD_init) - selling_price
      iteration <- iteration+1
    }
    #return NA if exceeding 1000 iterations
    if(iteration==1000) {return(NA)}
    return(SD_init)
  }

  Vol_vec <- vector(mode='numeric',length=0)
  for (i in 1:nrow(df)) {
    SD_i <- impliedVol(df$type[i],
                       df$futurePrice[i],
                       df$strike[i],
                       df$time_to_expiry[i],
                       r,df$optionPrice[i])
    Vol_vec <- c(Vol_vec, SD_i)
  }

  df$Implied_Volatility <- Vol_vec

  dfcall <- subset(df,type=='C')
  dfcall <- dfcall[order(dfcall$strike),]
  dfput <- subset(df,type=='P')
  dfput<- dfput[order(dfput$strike),]

  plot.new()
  par(mfrow=c(1,2))

  plot(dfcall$strike,dfcall$Implied_Volatility,
       type='o',col='blue',
       main='call future option',
       xlab='Strike',
       ylab='Implied Volatility')
  lines(dfcall$strike,dfcall$Implied_Volatility,col='blue')
  abline(v=dfcall$futurePrice[1],col='red')
  grid()

  plot(dfput$strike,dfput$Implied_Volatility,
       type='o',pch=1,cex=0.5,col='green',
       main='put future option',
       xlab='Strike',
       ylab='Implied Volatility')
  lines(dfput$strike,dfput$Implied_Volatility,col='green')
  abline(v=dfput$futurePrice[1],col='red')
  grid()

  return(df)
}
