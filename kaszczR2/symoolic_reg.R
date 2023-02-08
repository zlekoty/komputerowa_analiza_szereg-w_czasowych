library("gramEvol")


df <- read.csv("C:/Users/zleko/Desktop/studia/kaszcz/kaszczR2/PJM_Load_hourly.csv")
df$index <- c(1:length(df$PJM_Load_MW))
#View(df)
x <- df$index
y <- df$PJM_Load_MW

#plot(y)

loess50 <- loess(PJM_Load_MW~ index, data=df, span=.09)
smooth50 <- predict(loess50) 
plot(df$index, df$PJM_Load_MW, pch=0, main='Loess Regression Models')
lines(smooth50, x=df$index, col='red')


ruleDef <- list(expr = grule(op(expr, expr), func(expr), var),
                func = grule(sin, cos),
                op = grule('+', '-', '*'),
                var = grule(x))
grammarDef <- CreateGrammar(ruleDef)
grammarDef

SymRegFitFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (mean(log(1 + abs(y - result))))
}

#set.seed(314)
ge <- GrammaticalEvolution(grammarDef, SymRegFitFunc, terminationCost = 0.01, iterations = 1000, max.depth = 10)
ge # wynik
plot(y)
points(eval(ge$best$expressions), col = "red", type = "l")
#plot(x,cos(sin(x)) + x )
