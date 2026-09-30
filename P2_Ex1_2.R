library(reshape2)
library(ggplot2)
library(patchwork)
library(VGAM)

#Parte-2 1.

set.seed(123)  # Para reprodutibilidade
n <- 1000
beta <- 2  # Valor escolhido para β

# Gerar U ~ Uniforme(0,1)
u <- runif(n)

# Transformação inversa para obter as observações de X
x_inv <- beta * sqrt(-2 * log(1-u))


#Parte-2 2.
# Gerar n pares de variáveis normais padrão usando Box-Muller


#aplicar a transformação de Box-Muller para obter 2 normais N(0,1)
u1 <- runif(n)
u2 <- runif(n)
z1 <- sqrt(-2 * log(u1))*cos(2*pi*u2)
z2 <- sqrt(-2 * log(u1))*sin(2*pi*u2)

# Calcular o raio que segue a distribuição de Rayleigh
x_box <- beta*sqrt(z1^2 + z2^2)

# Gerar 1000 observações usando a função rrayleigh
x_vgam <- rrayleigh(n, scale = beta)

# Comparar os resultados
# Criar sequência de valores para a curva teórica
x_vals <- seq(0, max(c(x_inv, x_box, x_vgam)), length.out = 100)
dens_teorica <- drayleigh(x_vals, scale = beta)

# Plotar os histogramas e sobrepôr a curva teórica
par(mfrow = c(1,3))  # Dividir a janela gráfica em 3 colunas

hist(x_inv, probability = TRUE, main = "Transformação Inversa", 
     col = "lightblue", breaks = 30, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)

hist(x_box, probability = TRUE, main = "Box-Muller", 
     col = "lightgreen", breaks = 30, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)

hist(x_vgam, probability = TRUE, main = "VGAM rrayleigh", 
     col = "lightcoral", breaks = 60, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)


cat("T. Inv. (Média,Mediana, Desvio Padrão):  ", mean(x_inv),median(x_inv),sd(x_inv))
cat("B Mull. (Média,Mediana, Desvio Padrão):  ", mean(x_box),median(x_box),sd(x_box))
cat("T. Inv. (Média,Mediana, Desvio Padrão):  ", mean(x_vgam),median(x_vgam),sd(x_vgam))
cat("Dist. Teórica (Média,Mediana, Desvio Padrão): ", beta*sqrt(pi/2),beta*sqrt(2*log(2)),beta*sqrt((4-pi)/2))

#Em todos os métodos as estatísticas comuns (média, mediana, desvio padrão) estão condizentes com a da
#distribuição teórica


# Restaurar a janela gráfica para 1 gráfico por vez
par(mfrow = c(1,1))

ks.test(x_inv, "prayleigh", scale = beta)  # Transformação inversa
ks.test(x_box, "prayleigh", scale = beta)  # Box-Muller
ks.test(x_vgam, "prayleigh", scale = beta) # VGAM (referência)

#parece que a transformação inversa teve o melhor desempenho com n=1000

#mas será que é sempre assim?

# Parâmetros
betas <- seq(0.5, 5, by = 0.1)
n <- 1000  # Defina aqui o número de observações

# Função para simular e calcular os p-values do teste KS
simulate_ks <- function(seed, betas, n) {
  results <- data.frame(beta = betas, p_inv = NA, p_box = NA, p_vgam = NA, best_method = NA)
  
  for (i in seq_along(betas)) {
    set.seed(seed)
    beta <- betas[i]
    
    # Métodos de geração
    u <- runif(n)
    x_inv <- beta * sqrt(-2 * log(1-u))
    u1 <- runif(n)
    u2 <- runif(n)
    z1 <- sqrt(-2 * log(u1))*cos(2*pi*u2)
    z2 <- sqrt(-2 * log(u1))*sin(2*pi*u2)
    
    # Calcular o raio que segue a distribuição de Rayleigh
    x_box <- beta*sqrt(z1^2 + z2^2)
    x_vgam <- rrayleigh(n, scale = beta)
    
    # Teste KS de comparação com a CDF teórica 'prayleigh'
    ks_inv  <- ks.test(x_inv,  "prayleigh", scale = beta)
    ks_box  <- ks.test(x_box,  "prayleigh", scale = beta)
    ks_vgam <- ks.test(x_vgam, "prayleigh", scale = beta)
    
    # Armazenar p-values e identificar o melhor método
    results$p_inv[i]  <- ks_inv$p.value
    results$p_box[i]  <- ks_box$p.value
    results$p_vgam[i] <- ks_vgam$p.value
    
    pvals <- c(Transformation_Inverse = results$p_inv[i],
               Box_Muller = results$p_box[i],
               VGAM = results$p_vgam[i])
    results$best_method[i] <- names(which.max(pvals))
  }
  return(results)
}

# Obter resultados para as duas seeds
results_123 <- simulate_ks(123, betas, n)
results_321 <- simulate_ks(321, betas, n)

# Transformar para o formato "long"
results_long1 <- melt(results_123, id.vars = c("beta", "best_method"),
                      measure.vars = c("p_inv", "p_box", "p_vgam"),
                      variable.name = "Method", value.name = "p_value")
levels(results_long1$Method) <- c("Transformação Inversa", "Box-Muller", "VGAM")

results_long2 <- melt(results_321, id.vars = c("beta", "best_method"),
                      measure.vars = c("p_inv", "p_box", "p_vgam"),
                      variable.name = "Method", value.name = "p_value")
levels(results_long2$Method) <- c("Transformação Inversa", "Box-Muller", "VGAM")

# Criar os gráficos para cada seed
plot1 <- ggplot(results_long1, aes(x = beta, y = p_value, color = Method)) +
  geom_line() + geom_point() +
  labs(title = "Comparação dos p-values (seed = 123)", x = "β", y = "p-value") +
  theme_minimal()

plot2 <- ggplot(results_long2, aes(x = beta, y = p_value, color = Method)) +
  geom_line() + geom_point() +
  labs(title = "Comparação dos p-values (seed = 321)", x = "β", y = "p-value") +
  theme_minimal()

# Combinar os gráficos num único plot
combined_plot <- plot1 / plot2
print(combined_plot)



#Agora deixamos beta = 2 fixo e deixamos o número de amostras variar. fazemos isto para duas sementes diferentes

# Parâmetros fixos e variáveis
beta <- 2
M_values <- seq(200, 5000, by = 200)

# Função para simular os dados e realizar os testes KS
simulate_results <- function(seed_val) {
  results <- data.frame(M = M_values, p_inv = NA, p_box = NA, p_vgam = NA, best_method = NA)
  
  for (i in seq_along(M_values)) {
    set.seed(seed_val)
    M <- M_values[i]
    
    #Transformação Inversa
    u <- runif(M)
    x_inv <- beta * sqrt(-2 * log(1-u))
    
    u1 <- runif(M)
    u2 <- runif(M)
    z1 <- sqrt(-2 * log(u1))*cos(2*pi*u2)
    z2 <- sqrt(-2 * log(u1))*sin(2*pi*u2)
    
    # Calcular o raio que segue a distribuição de Rayleigh
    x_box <- beta*sqrt(z1^2 + z2^2)
    
    # Método 3: Usando a função rrayleigh do pacote VGAM
    x_vgam <- rrayleigh(M, scale = beta)
    
    # Teste KS para cada método (usando a CDF teórica 'prayleigh')
    ks_inv  <- ks.test(x_inv,  "prayleigh", scale = beta)
    ks_box  <- ks.test(x_box,  "prayleigh", scale = beta)
    ks_vgam <- ks.test(x_vgam, "prayleigh", scale = beta)
    
    results$p_inv[i]  <- ks_inv$p.value
    results$p_box[i]  <- ks_box$p.value
    results$p_vgam[i] <- ks_vgam$p.value
    
    # Determinar qual o método teve o maior p-value
    pvals <- c(Transformation_Inverse = results$p_inv[i],
               Box_Muller = results$p_box[i],
               VGAM = results$p_vgam[i])
    results$best_method[i] <- names(which.max(pvals))
  }
  return(results)
}

# Obter resultados para cada seed
results_123 <- simulate_results(123)
results_321 <- simulate_results(321)

# Transformar os data frames para o formato "long"
results_long1 <- melt(results_123, id.vars = c("M", "best_method"), 
                      measure.vars = c("p_inv", "p_box", "p_vgam"), 
                      variable.name = "Method", value.name = "p_value")
levels(results_long1$Method) <- c("Transformação Inversa", "Box-Muller", "VGAM")

results_long2 <- melt(results_321, id.vars = c("M", "best_method"), 
                      measure.vars = c("p_inv", "p_box", "p_vgam"), 
                      variable.name = "Method", value.name = "p_value")
levels(results_long2$Method) <- c("Transformação Inversa", "Box-Muller", "VGAM")

# Criar os gráficos para cada seed
plot1 <- ggplot(results_long1, aes(x = M, y = p_value, color = Method)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparação dos p-values (seed = 123)",
       x = "Número de Observações (M)", y = "p-value") +
  theme_minimal()

plot2 <- ggplot(results_long2, aes(x = M, y = p_value, color = Method)) +
  geom_line() +
  geom_point() +
  labs(title = "Comparação dos p-values (seed = 321)",
       x = "Número de Observações (M)", y = "p-value") +
  theme_minimal()

# Combinar os dois gráficos em um único plot (empilhados verticalmente)
combined_plot <- plot1 / plot2

# Exibir o gráfico combinado
print(combined_plot)




#agora testamos fazendo set.seed(123) antes de cada método e usando beta * sqrt(-2 * log(u)) no método da
#transformação inversa em vez de beta * sqrt(-2 * log(1-u))


set.seed(123)  # Para reprodutibilidade
n <- 1000
beta <- 2  # Valor escolhido para β

# Gerar U ~ Uniforme(0,1)
u <- runif(n)

# Transformação inversa para obter as observações de X
x_inv <- beta * sqrt(-2 * log(u))


#Parte-2 2.
set.seed(123)  # Para reprodutibilidade
# Gerar n pares de variáveis normais padrão usando Box-Muller


#aplicar a transformação de Box-Muller para obter 2 normais N(0,1)
u1 <- runif(n)
u2 <- runif(n)
z1 <- sqrt(-2 * log(u1))*cos(2*pi*u2)
z2 <- sqrt(-2 * log(u1))*sin(2*pi*u2)

# Calcular o raio que segue a distribuição de Rayleigh
x_box <- beta*sqrt(z1^2 + z2^2)

# Gerar 1000 observações usando a função rrayleigh
set.seed(123)  # Para reprodutibilidade
x_vgam <- rrayleigh(n, scale = beta)

# Comparar os resultados
# Criar sequência de valores para a curva teórica
x_vals <- seq(0, max(c(x_inv, x_box, x_vgam)), length.out = 100)
dens_teorica <- drayleigh(x_vals, scale = beta)

# Plotar os histogramas e sobrepor a curva teórica
par(mfrow = c(1,3))  # Dividir a janela gráfica em 3 colunas

hist(x_inv, probability = TRUE, main = "Transformação Inversa", 
     col = "lightblue", breaks = 30, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)

hist(x_box, probability = TRUE, main = "Box-Muller", 
     col = "lightgreen", breaks = 30, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)

hist(x_vgam, probability = TRUE, main = "VGAM rrayleigh", 
     col = "lightcoral", breaks = 30, xlab = "x")
lines(x_vals, dens_teorica, col = "red", lwd = 2)

# Restaurar a janela gráfica para 1 gráfico por vez
par(mfrow = c(1,1))

ks.test(x_inv, "prayleigh", scale = beta)  # Transformação inversa
ks.test(x_box, "prayleigh", scale = beta)  # Box-Muller
ks.test(x_vgam, "prayleigh", scale = beta) # VGAM (referência)






