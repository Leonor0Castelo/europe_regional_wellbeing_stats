# Carregar as bibliotecas necessárias
library(boot)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)

# Carregar os dados
url <- "https://web.tecnico.ulisboa.pt/~ist13493/LECD2025/Projecto/BemEstarEuropa.xlsx"
download.file(url, destfile = "BemEstarEuropa.xlsx", mode = "wb")
dados<-read_xlsx("BemEstarEuropa.xlsx", sheet=1)

# Selecionar a variável de interesse e remover NA's
rendimento <- na.omit(dados$rendimento)
desemprego <- na.omit(dados$desemprego)

#teste de normalidade 

shapiro.test(rendimento)
shapiro.test(desemprego)

# Criar boxplot do Rendimento
plot_rend <- ggplot(data.frame(Rendimento = rendimento), aes(y = Rendimento)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot do Rendimento", y = "Rendimento") +
  theme_minimal()

# Criar boxplot do Desemprego
plot_desemp <- ggplot(data.frame(Desemprego = desemprego), aes(y = Desemprego)) +
  geom_boxplot(fill = "tomato", color = "black") +
  labs(title = "Boxplot do Desemprego", y = "Desemprego") +
  theme_minimal()

#Juntar os dois plots lado a lado
plot_rend + plot_desemp

# 1. Usar a biblioteca boot com funções que retornam o estimador e o desvio padrão


# Função para calcular a média aparada a 5% e estimar o desvio padrão via nested bootstrap
media_aparada_stud <- function(data, indices) {
  sample_data <- data[indices]
  stat <- mean(sample_data, trim = 0.05)
  
  # Nested bootstrap para estimar o desvio padrão
  B_inner <- 200   # número de reamostragens internas (pode ser ajustado)
  n <- length(sample_data)
  boot_inner <- numeric(B_inner)
  for(j in 1:B_inner){
    inner_indices <- sample(1:n, size = n, replace = TRUE)
    boot_inner[j] <- mean(sample_data[inner_indices], trim = 0.05)
  }
  se_est <- sd(boot_inner)
  
  # Retorna um vetor com o estimador e o desvio padrão
  return(c(stat, se_est))
}

# Função para calcular a mediana e estimar o erro padrão via nested bootstrap
mediana_stud <- function(data, indices) {
  sample_data <- data[indices]
  stat <- median(sample_data)
  
  # Nested bootstrap para estimar var(T_n) para cada uma das amostras
  B_inner <- 200   # número de reamostragens internas
  n <- length(sample_data)
  boot_inner <- numeric(B_inner)
  for(j in 1:B_inner){
    inner_indices <- sample(1:n, size = n, replace = TRUE)
    boot_inner[j] <- median(sample_data[inner_indices])
  }
  se_est <- sd(boot_inner)
  
  # Retorna um vetor com o estimador var(T_n)
  return(c(stat, se_est))
}

# Número de reamostragens externas para o bootstrap
R_boot <- 10^3
set.seed(123) #para reprodutibilidade

# Aplicar o bootstrap para a média aparada com a nova função
media_apar_boot <- boot(data = rendimento, statistic = media_aparada_stud, R = R_boot) 

# Aplicar o bootstrap para a mediana com a nova função
mediana_boot <- boot(data = rendimento, statistic = mediana_stud, R = R_boot)

# Calcular intervalos de confiança utilizando o método studentized
media_apar_ci_boot <- boot.ci(media_apar_boot, type = "stud")
mediana_ci_boot    <- boot.ci(mediana_boot, type = "stud")

cat("Intervalo de Confiança para a Média Aparada a 5%:\n")
print(media_apar_ci_boot)

cat("\nIntervalo de Confiança para a Mediana:\n")
print(mediana_ci_boot)




#2. Implementação manual do método bootstrap percentil-t (studentized)

# Função para realizar o nested bootstrap e retornar o desvio padrão estimado para um conjunto de dados
bootstrap_se <- function(data, stat_func, b) {
  # b: número de reamostragens internas para estimar o desvio padrão
  boot_stats <- numeric(b)
  n <- length(data)
  for(i in 1:b){
    indices_inner <- sample(1:n, size = n, replace = TRUE)
    boot_stats[i] <- stat_func(data[indices_inner])
  }
  return(sd(boot_stats))
}

# Função que implementa o algoritmo do percentil-t
# stat_func: função que calcula o estimador (ex.: média aparada ou mediana)
# data: vetor original de dados
# B: número de reamostragens externas
# b: número de reamostragens internas para estimar o se
bootstrap_t_ci <- function(data, stat_func, B, b, conf = 0.95) {
  n <- length(data)
  
  # Estimador original e seu se (calculado via nested bootstrap)
  theta0 <- stat_func(data)
  se0 <- bootstrap_se(data, stat_func, b)
  
  # Vetor para armazenar os  valores da estatística t de cada reamostragem
  t_stats <- numeric(B)
  # Armazenar os estimadores de cada replicação (opcional, se desejar conferir)
  theta_bs <- numeric(B)
  
  for(i in 1:B){
    # Reamostragem externa
    indices <- sample(1:n, size = n, replace = TRUE)
    sample_boot <- data[indices]
    
    theta_i <- stat_func(sample_boot)
    theta_bs[i] <- theta_i
    # Calcular o desvio padrão para a reamostragem (nested bootstrap)
    se_i <- bootstrap_se(sample_boot, stat_func, b)
    
    # Evitar divisão por zero
    if(se_i == 0){
      t_stats[i] <- NA
    } else {
      t_stats[i] <- (theta_i - theta0) / se_i
    }
  }
  
  # Remover eventuais NA's (se ocorrer divisão por zero em alguma replicação)
  t_stats <- na.omit(t_stats)
  
  # Calcular os quantis para a distribuição da estatística t
  alpha <- 1 - conf
  t_lower <- quantile(t_stats, probs = 1 - alpha/2)
  t_upper <- quantile(t_stats, probs = alpha/2)
  
  # Intervalo de confiança:
  ci_lower <- theta0 - t_lower * se0
  ci_upper <- theta0 - t_upper * se0
  
  return(list(theta0 = theta0, se0 = se0, ci = c(lower = ci_lower, upper = ci_upper)))
}

B_manual <- 1000   # replicações externas
b_manual <- 200    # replicações internas (nested)

# Cálculo para a média aparada a 5%
ci_media_aparada_manual <- bootstrap_t_ci(rendimento, 
                                          function(x) mean(x, trim = 0.05),
                                          B = B_manual, 
                                          b = b_manual,
                                          conf = 0.95)

# Cálculo para a mediana
ci_mediana_manual <- bootstrap_t_ci(rendimento, 
                                    median,
                                    B = B_manual, 
                                    b = b_manual,
                                    conf = 0.95)

cat("\nImplementação Manual - Intervalo de Confiança para a Média Aparada a 5%:\n")
print(ci_media_aparada_manual$ci)
print(ci_media_aparada_manual$theta0)
print(ci_media_aparada_manual$se0)

cat("\nImplementação Manual - Intervalo de Confiança para a Mediana:\n")
print(ci_mediana_manual$ci)
print(ci_mediana_manual$theta0)
print(ci_mediana_manual$se0)


#fazemos uma análise parecida para o Desemprego que parece ter outliers

set.seed(321) #para reprodutibilidade

# Aplicar o bootstrap para a média aparada com a nova função
media_apar_boot <- boot(data = desemprego, statistic = media_aparada_stud, R = R_boot) 

# Aplicar o bootstrap para a mediana com a nova função
mediana_boot <- boot(data = desemprego, statistic = mediana_stud, R = R_boot)

# Calcular intervalos de confiança utilizando o método studentized (bootstrap-t)
media_apar_ci_boot <- boot.ci(media_apar_boot, type = "stud")
mediana_ci_boot    <- boot.ci(mediana_boot, type = "stud")

cat("Intervalo de Confiança para a Média Aparada a 5%:\n")
print(media_apar_ci_boot)

cat("\nIntervalo de Confiança para a Mediana:\n")
print(mediana_ci_boot)

#nosso algoritmo


# Cálculo para a média aparada a 5%
ci_media_aparada_manual <- bootstrap_t_ci(desemprego, 
                                          function(x) mean(x, trim = 0.05),
                                          B = B_manual, 
                                          b = b_manual,
                                          conf = 0.95)

# Cálculo para a mediana
ci_mediana_manual <- bootstrap_t_ci(desemprego, 
                                    median,
                                    B = B_manual, 
                                    b = b_manual,
                                    conf = 0.95)

cat("\nImplementação Manual - Intervalo de Confiança para a Média Aparada a 5% (bootstrap-t):\n")
print(ci_media_aparada_manual$ci)
print(ci_media_aparada_manual$theta0)
print(ci_media_aparada_manual$se0)

cat("\nImplementação Manual - Intervalo de Confiança para a Mediana (bootstrap-t):\n")
print(ci_mediana_manual$ci)
print(ci_mediana_manual$theta0)
print(ci_mediana_manual$se0)


