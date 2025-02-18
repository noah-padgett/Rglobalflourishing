
df.imp.i <- df.imp$imp.res[[1]]

ii = "HAPPY_W2"
vi = as.formula(paste0(" ~ ", ii, " | .imp"))
densityplot(df.imp.i, vi)

plot_dat <- complete(df.imp.i, action = "long",include="TRUE")

dat0 <- plot_dat %>% filter(.imp == 0)
dat1 <- plot_dat %>% filter(.imp > 0)
p <- dat1 %>%
  ggplot(aes(x=.data[[ii]], group=.imp))+
    geom_density(adjust=2, color="black")+
    geom_density(adjust=2, color="red", data=dat0) +
    theme_classic()
p

mymod <- reformulate(
  response = "ici(df.imp.i)",
  termlabels = attr(terms(df.imp.i$formulas[[ii]]), "term.labels")
)
fit <- with(df.imp.i , glm(mymod, family = binomial))
ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)), imp$m + 1)

