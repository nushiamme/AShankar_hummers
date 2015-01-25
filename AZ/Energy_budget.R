## Below all for broad-billed hummingbirds
## Anusha Shankar January 21, 2015


## Thermoregulatory equations
## tre_h = 0.0144*Te - 0.2655; intercept got by substituting BMR~BBLH~ in equation with Costa's slope, and UCT = 35&deg;C
## tre_l = 0.9530 - 0.0223*Te, assuming LCT is 32&deg;C
## Tb from the tre_l equation   

## Metabolic rates in ml O~2~/h
bmr <- 0.2385*60
rmr <- 1.5*bmr
hmr <- 10.3*bmr
flmr <- 0.5*hmr

## From SC daytime temperature data and broad-bill equation (in csv)
tre_h <- 135.8

## From SC daytime temperature data and broad-bill equation (in csv)
tre_l <-121.3
bmr <- 14.3
tre_total <- (tre_h+tre_l+bmr)

## NEE in ml O2/h
nee <- 218.9

## ACT = 70% resting + 15% hovering + 15% flying; assuming 14 daylight hours, in ml O~2~/h
ACT <- (0.7*14*(rmr-bmr)) + (0.15*14*(hmr-bmr)) + (0.15*14*(flmr-bmr))
ACT

DEE_model <- ACT + nee + tre_total
DEE_model

## To get a per hour CO2 estimate, multiply by RQ and divide by 24
DEE_model_hr <- DEE_model*0.85/24
