library(lavaan)
library(ggplot2)
library(ggthemes)
library(ggdag)
library(dagitty)
library(viridis)


fd <- function(n, a, b, c, d, g, h, j, k, l, u1, u2) {
  U <- rnorm(n,10,1)
  P <- rnorm(n, 12, sd = 1)
  X <- rnorm(n, 5, sd = 1)
  I <- rnorm(n, 25, sd = 1)
  e_m <- rnorm(n,0,1)
  e_y <- rnorm(n,0,1)
  e_s <- rnorm(n,0,1)
  S <- 14 + u1*U + e_s
  M <- a*S + b*I + c*P + d*X + e_m
  Y <- g*M + j*I + k*P + l*X + u2*U + e_y
  
  
  df1 <- data.frame(X, I, P, S, M, Y, U)
  
  fdmodel <- "M ~ a*S + b*I + c*P + d*X
              Y ~ g*M + h*S + j*I + k*P + l*X
              ATE := a*g"
  
  
  fd <- sem(fdmodel,df1)@ParTable$est[22]
  unadj <- coef(lm(Y~S))[2]
  adj <- coef(lm(Y~S+U))[2]
  return(c(fd,unadj,adj))
}

res <- data.frame(t(replicate(5000,fd(1000, 0.875, 0.5, 0.5, 0.5, 0.875, 0.5, 0.5, 0.5, 0.5, 0.8, 0.1 ))))
names(res) <- c("fd","unadj","adj")

# Front door criterion estimate
mean(res$fd)
# Correct estimate
mean(res$adj)
# Biased estimated
mean(res$unadj)

ggplot(res,aes(x=fd)) + 
  geom_density(alpha =0.8, fill = "gray", color = "gray") + 
  theme_classic() + 
  geom_vline(xintercept = mean(res$fd), linetype = 1, color = "red") + 
  annotate("text", x = 0.72, y = 5, family = 'serif', label = "Estimated causal effect", color = "red") +
  xlab("Estimated average treatment effect") + 
  ylab("Density") + 
  theme(text=element_text(family = 'serif', color = 'black')) +
  coord_cartesian(xlim=c(0.6, 0.95))
  #ggtitle("Parameter estimates using front-door criterion")

ggplot(res,aes(x=adj)) + 
  geom_density(alpha = 0.8, fill = "gray", color = "gray") + 
  theme_classic() + 
  geom_vline(xintercept = mean(res$adj), linetype = 1, color = "red") + 
  annotate("text", x = 0.68, y = 5.2, family = 'serif', label = "True causal effect", color = "red") +
  xlab("Estimated average treatment effect") + 
  ylab("Density") +
  theme(text=element_text(family = 'serif', color = 'black')) +
  coord_cartesian(xlim=c(0.4, 1.125)) 
  #ggtitle("Parameter estimates using adjustment")

ggplot(res,aes(x=unadj)) + 
  geom_density(alpha = 0.8, fill = "gray", color = "gray") + 
  theme_classic() + 
  geom_vline(xintercept = mean(res$unadj), linetype = 1, color = "red") + 
  annotate("text", x = 0.72, y = 5, family = 'serif', label = "Biased effect estimate", color = "red") +
  xlab("Estimated average treatment effect") + 
  ylab("Density") + 
  theme(text=element_text(family = 'serif', color = 'black')) +
  coord_cartesian(xlim=c(0.5, 1.125)) 
  #ggtitle("Unadjusted parameter estimates")