# simulacao
swiss <- datasets::swiss
x <- model.matrix(Fertility~., swiss)[,-1] # transforma em matriz e tira coluna var dependente
y <- swiss$Fertility
lambda <- 10^seq(10, -2, length = 100) # pode escolher default 

vetor_seed <- 1:1000

# cria matriz vazia para receber os valores de teste
mat_resultados <- matrix(NA, nrow = 1000, ncol = 3)
names(mat_resultados) <- c("reg_normal", "ridge", "lasso")

for (i in seq_along(vetor_seed)){
  set.seed(i)
  train = sample(1:nrow(x), nrow(x)/2)
  test = (-train)
  ytest = y[test]
  # rodar sem o lambda e com lambda.min (ridge e lasso) para testar o erro  
  swisslm <- lm(Fertility~., data = swiss, subset = train)
  s.pred <- predict(swisslm, newdata = swiss[test,])
  mat_resultados[i, 1] <- mean((s.pred-ytest)^2) # o erro para uma regressão normal
  
  ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0) # para achar o melhor lamb (s)
  #plot(cv.out) 
  cv.out$lambda.min # esse será o lamb
  bestlam <- cv.out$lambda.min
  ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
  #ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,]) # se bota 0 no lambda fica regr normal 
  mat_resultados[i, 2] <- mean((ridge.pred-ytest)^2) # o erro para regressão com ridge (regularização) e melhor lambda 
  
  lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
  lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
  mat_resultados[i, 3] <- mean((lasso.pred-ytest)^2)
  
}

mat_resultados %<>% as.data.frame() 

mat_resultados_gather <- mat_resultados %>% 
  gather(regressao, erro)

ggplot(mat_resultados_gather, aes(x = erro, colour = regressao, fill = regressao)) + 
  geom_density(alpha = 0.1)

ggplot(mat_resultados_gather, aes(erro)) +
  facet_wrap(~regressao, scales = 'free_x') + 
  geom_histogram(binwidth = 5)

# Lasso foi o pior resultado
# Fazer regularização ou não com ridge é indiferente para ESSA BASE de dados.
