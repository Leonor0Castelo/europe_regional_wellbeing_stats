library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

url <- "https://web.tecnico.ulisboa.pt/~ist13493/LECD2025/Projecto/BemEstarEuropa.xlsx"
download.file(url, destfile = "BemEstarEuropa.xlsx", mode = "wb")
dados<-read_xlsx("BemEstarEuropa.xlsx", sheet=1)


# Hipótese 1: Relação entre Nível de Educação e Taxa de Desemprego
# H0: Não existe associação significativa entre Educação e Taxa de Desemprego.
# H1: Existe associação significativa, com maior nível de educação associado a menor taxa de desemprego.


ggplot(dados, aes(x = educação, y = desemprego)) +
  geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relação entre Nível de Educação e Taxa de Desemprego",
       x = "Nível de Educação",
       y = "Taxa de Desemprego") +
  theme_minimal()

# Verificar normalidade
shapiro.test(dados$educação)
shapiro.test(dados$desemprego)

#são significativamente diferentes de uma distribuição normal

# usamos o teste de correlação de Spearman:
resultado_hip1 <- cor.test(dados$educação, dados$desemprego, method = "spearman")
print(resultado_hip1)

#valor p muito baixo e estimativa rho -0.50 sugere que, conforme a educação aumenta, o desemprego
#tende a diminuir (o que faz sentido)


# Hipótese 2: Relação entre Rendimento e Satisfação com a Vida
# H0: Regioões com rendimentos mais baixos apresentam níveis de satisfação menores ou iguais a pessoas com rendimentos superiores
# H1: Regiões com rendimentos mais altos apresentam níveis de satisfação superiores.



# Dividir os dados em dois grupos (ex.: rendimentos "Alto" e "Baixo") com base na mediana e comparar a satisfação:
dados$grupoRendimento <- ifelse(dados$rendimento >= median(dados$rendimento, na.rm = TRUE), "Alto", "Baixo")

# Verificar normalidade da variável Satisfação em cada grupo
shapiro.test(dados$satisfação[dados$grupoRendimento == "Alto"])
shapiro.test(dados$satisfação[dados$grupoRendimento == "Baixo"])

#apesar de o valor p em shapiro.test(dados$satisfação[dados$grupoRendimento == "Alto"]) ser < 0.01 (não muito)
# shapiro.test(dados$satisfação[dados$grupoRendimento == "Baixo"]) tem valor p 0.37 >0.01


#Usamos o teste t

t.test(dados$satisfação[dados$grupoRendimento == "Alto"],dados$satisfação[dados$grupoRendimento == "Baixo"],paired = FALSE,alternative = "greater")

#valor p praticamente 0, rejeitamos a hipótese nula, pelo que há evidência de que a satisfação
#é mais alta em grupos de maior rendimento (o que até certo ponto é esperado)

#por curiosidade, note-se que o teste de Mann-Whitney leva à mesma conclusão
wilcox.test(dados$satisfação[dados$grupoRendimento == "Alto"], 
            dados$satisfação[dados$grupoRendimento == "Baixo"],paired = FALSE, 
            alternative = "greater")



#Hipótese 3:
#H0: A esperança de vida à nascença é menor ou igual em regiões de alta poluição do ar em comparação com baixa.
#H1: A esperança de vida à nascença é maior em regiões com menor poluição do ar em comparação com regiões com alta poluição.

# Criar um grupo baseado na mediana da poluição: 
dados$grupoPoluicao <- ifelse(dados$poluição <= median(dados$poluição, na.rm = TRUE), 
                              "Baixa", "Alta")

# Verificar normalidade em cada grupo 
shapiro.test(dados$vida[dados$grupoPoluicao == "Baixa"])
shapiro.test(dados$vida[dados$grupoPoluicao == "Alta"])

# Se as variáveis não forem normais, usamos o teste de Mann-Whitney, tendo em conta que os dados:
#não são pareados, por se referirem a regiões distintas

wilcox.test(dados$vida[dados$grupoPoluicao == "Baixa"], 
            dados$vida[dados$grupoPoluicao == "Alta"],paired = FALSE, 
            alternative = "greater")

#com um nível de significância de 0.05, concluimos que a espectativa de vida do grupo com menor poluição
#é maior do que o com maior poluição, embora por pouco


#Hipótese 4:
#H0: Países do Sul da Europa apresentam níveis de satisfação iguais ou inferiores aos de outras regiões europeias.
#H1:Países do Sul da Europa apresentam níveis de satisfação com a vida superiores aos de outras regiões europeias.

# Criar uma variável que distingue países do Sul da Europa de outros:
dados$grupoRegiao <- ifelse(dados$continente == "Southern Europe","Sul da Europa", "Outras")

# Verificar normalidade
shapiro.test(dados$satisfação[dados$grupoRegiao == "Sul da Europa"])
shapiro.test(dados$satisfação[dados$grupoRegiao == "Outras"])

wilcox.test(dados$satisfação[dados$grupoRegiao == "Sul da Europa"], 
            dados$satisfação[dados$grupoRegiao == "Outras"],paired = FALSE, 
            alternative = "greater")

#estes resultados sugerem que não há evidências estatísticas para afirmar que a satisfação
#com a vida nos países do Sul da Europa é maior do que nas outras regiões europeias. 
#e que a satisfação nos outros países é maior ou igual que no sul da europa

#Hipótese 5:
# H0: O terceiro quartil do rendimento é maior ou igual que 20000
# H1: O terceiro quartil do rendimento é menor do que 20000

# Teste do sinal: Verificar se o terceiro quartil do rendimento é superior a 20000
# Usamos o teste para verificar se Q3 > 20000

# Para aplicar o teste de sinal, vamos criar uma diferença entre cada valor de rendimento e 20000



 s <- sum(dados$rendimento>20000)
 #tirar repetições
 n_novo<-length(dados$rendimento)-sum(dados$rendimento==20000)
 #assumindo a hipótese nula, temos distribuição de Sn -> Bin(n=length(dados$rendimento),p=0.25) ou até com
 # um p menor, que levaria a valores p ainda maiores
 p_value<-pbinom(s,n_novo,p=0.25,lower.tail = TRUE)
 p_value
 
 #aceitamos a hipótese de que o terceiro quartil tem rendimento > 20000.
 
 #Hipótese 6:
 #H0: As distribuições relativas à existência de apoio social são idênticas nas 3 regiões da europa (central, sul, oeste)
 #H1: Pelo menos duas das distribuições diferem na localização
 
hist(dados$social[dados$continente=="Southern Europe"],main = "Southern Europe", xlab = "social", col = "green", border = "black", breaks = 20)
 
 
hist(dados$social[dados$continente=="Central Europe"],main = "Central Europe", xlab = "social", col = "red", border = "black", breaks = 20)
 
 
hist(dados$social[dados$continente=="Western Europe"],main = "Western Europe", xlab = "social", col = "blue", border = "black", breaks = 20)
 
 
 #assumimos, de forma simplificada, que as ditribuições  têm a mesma forma e variabilidade (apesar de)
 #de termos poucos dados e uma análise dos seus histogramas não poder confirmar isso com 100% de certeza
 sul = dados$social[dados$continente=="Southern Europe"]
 central = dados$social[dados$continente=="Central Europe"]
 oeste = dados$social[dados$continente=="Western Europe"]
 
 # Realizar o teste de Kruskal-Wallis
 resultado <- kruskal.test(list(sul, central, oeste))
 
 print(resultado)
 
 #Hipótese 7:
 # H0: Não existe associação significativa entre educação e voto
 # H1: Existe associação significativa entre educação e voto
 
 #faremos testes para várias regiões 
 
 
 # Dividir os dados por região e aplicar o teste de Spearman em cada grupo
 resultados <- lapply(split(dados, dados$continente), function(subdf) {
   teste <- cor.test(subdf$educação, subdf$voto, method = "spearman")
   data.frame(região = unique(subdf$região),
              continente = subdf$continente,
              rho = teste$estimate,
              p.value = teste$p.value)
 })
 
 # Combinar os resultados num único data frame
 resultados_df <- do.call(rbind, resultados)
 
 print(resultados_df)
 
 # Agregar os dados para que cada região tenha um único p-value (por exemplo, a média dos p-values)
 resultados_df_agg <- resultados_df %>%
   group_by(continente) %>%  
   summarise(p.value = mean(p.value)) %>%
   ungroup() %>%
   mutate(color = ifelse(p.value <= 0.10, "red", "green")) %>%
   mutate(decisao = ifelse(p.value <= 0.10, "Rejeitar H0", "Não Rejeitar H0"))
  
 
 
 # Criar coluna para definir a cor condicionalmente
 resultados_df_agg <- resultados_df_agg %>%
   mutate(color = ifelse(p.value <= 0.10, "red", "green"))
 
 # Criar o gráfico
 ggplot(resultados_df_agg, aes(x = continente, y = p.value, fill = decisao)) +
   geom_col() + 
   scale_fill_manual(values = c("Rejeitar H0" = "red", "Não Rejeitar H0" = "green"),
                     name = "Decisão") +
   geom_hline(yintercept = 0.10, linetype = "dashed", color = "black") +
   labs(title = "p-values por Região",
        x = "Região",
        y = "p-value") +
   theme_minimal()
 
 