library(readxl)
library(ggplot2)
library(dbplyr)
library(dplyr)
library(ggcorrplot)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)  # Para paletas de cor perceptualmente uniformes

url <- "https://web.tecnico.ulisboa.pt/~ist13493/LECD2025/Projecto/BemEstarEuropa.xlsx"
download.file(url, destfile = "BemEstarEuropa.xlsx", mode = "wb")
dados<-read_xlsx("BemEstarEuropa.xlsx", sheet=1)
head(dados)
summary(dados)

tabela_paises <- dados %>%
  group_by(país) %>%
  summarise(n_regioes = n())
print(tabela_paises)

#para termos apenas os dados numéricos para usar para a correlação
dados_numericos <- dados[sapply(dados, is.numeric)]
correlacoes <-cor(dados_numericos, method = "pearson")
# Criar a matriz de correlação
matrix_correlação<-ggcorrplot(
  correlacoes,
  method="circle",
  type="lower",
  lab= TRUE,
  title= "Gráfico de Correlação (Método de Pearson)",
  ggtheme = theme_minimal()
)

# Mostrar o gráfico
print(matrix_correlação)


library(ggplot2)

# Função para gerar o gráfico de barras
gerar_grafico_barras <- function(dados,variavel) {
  media_por_continente <- aggregate(dados[[variavel]] ~ dados$continente, data = dados, FUN = mean)
  colnames(media_por_continente) <- c("Continente", "Media")
  media_por_continente <- media_por_continente[order(media_por_continente$Media), ]
  ggplot(media_por_continente, aes(x =reorder(Continente,Media), y = Media, fill = Continente)) +
    geom_bar(stat = "identity", color = "black", show.legend=FALSE) +
    labs(title = paste(variavel, "Média", "por Zona da Europa"),
         x = "Continente",
         y = paste("Média de", variavel)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

criar_tabela_comparativa <- function(dados, variavel){
  valores <- as.numeric(dados[[variavel]])
  estatisticas<-boxplot.stats(valores)
  outliers_valores<-estatisticas$out
  media_variavel <- mean(valores, na.rm = TRUE)
  # Identifica os países e as respetivas com valores muito acima da média
  acima<-dados %>%
    filter(valores %in% outliers_valores & valores > media_variavel) %>% # Comparar com a média
    select(país,região,!!sym(variavel)) %>% # Selecionar colunas desejadas
    arrange(desc(!!sym(variavel))) # Ordenar do maior para o menor
  
  # Identifica os países e as respetivas regiões com valores muito abaixo da média
  abaixo <- dados %>%
    filter(valores %in% outliers_valores & valores< media_variavel) %>% # Comparar com a média
    select(país, região, !!sym(variavel)) %>% # Selecionar colunas desejadas
    arrange(!!sym(variavel)) # Ordenar do menor para o maior
  
  # Criar a tabela final contendo os resultados de acima e abaixo
  tabela_final<-list(
    "Muito acima da média"=acima,
    "Muito abaixo da média"=abaixo
  )
  
  return(tabela_final)
}

criar_tabela_maior_indice<-function(dados, variavel) {
  tabela_maior_indice<-dados%>%
    arrange(desc(!!sym(variavel)))%>%
    select(país,região,!!sym(variavel))%>%
    slice(1:3)
  return(tabela_maior_indice)
}

criar_tabela_menor_indice<-function(dados, variavel) {
  tabela_menor_indice<-dados%>%
    arrange(!!sym(variavel))%>%
    select(país,região,!!sym(variavel))%>%
    slice(1:3)
  return(tabela_menor_indice)
}


#Educação
grafico_educação<-ggplot(dados,aes(x=reorder(país,educação), y =educação)) +
  geom_point(aes(y=max(educação,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(educação,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(educação,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=educação,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(educação, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
    ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(educação, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
    ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
      ),
    name="Legenda"
    ) +
  labs(
    title ="Distribuição da Percentagem de Adultos que têm pelo menos o Ensino Secundário",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Percentagem de Adultos que têm pelo menos o Ensino Secundário (%)"
    ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
    )

print(grafico_educação)

print(criar_tabela_maior_indice(dados,"educação"))

print(criar_tabela_menor_indice(dados,"educação"))

resultado_educacao <- criar_tabela_comparativa(dados, "educação")
print(resultado_educacao)
print(gerar_grafico_barras(dados,"educação"))
 
#Emprego
grafico_emprego<-ggplot(dados,aes(x=reorder(país,emprego), y =emprego)) +
  geom_point(aes(y=max(emprego,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(emprego,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(emprego,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=emprego,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(emprego, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(emprego, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da da Taxa de Emprego na Europa",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Taxa de Emprego (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_emprego)
print(criar_tabela_maior_indice(dados,"emprego"))
print(criar_tabela_menor_indice(dados,"emprego"))
resultado_emprego <- criar_tabela_comparativa(dados,"emprego")
print(resultado_emprego)
print(gerar_grafico_barras(dados,"emprego"))

#Desemprego
grafico_desemprego<-ggplot(dados,aes(x=reorder(país,desemprego), y =desemprego)) +
  geom_point(aes(y=max(desemprego,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(desemprego,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(desemprego,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=desemprego,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(desemprego, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(desemprego, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da Taxa de Desemprego na Europa",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Taxa de Desemprego (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_desemprego)
print(criar_tabela_maior_indice(dados,"desemprego"))
print(criar_tabela_menor_indice(dados,"desemprego"))
resultado_desemprego <- criar_tabela_comparativa(dados, "desemprego")
print(resultado_desemprego)
print(gerar_grafico_barras(dados,"desemprego"))



#Rendimento
grafico_rendimento<-ggplot(dados,aes(x=reorder(país,rendimento), y =rendimento)) +
  geom_point(aes(y=max(rendimento,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(rendimento,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(rendimento,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=rendimento,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(rendimento, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(rendimento, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição do Rendimento médio na Europa",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Rendimento (USD)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_rendimento)
print(criar_tabela_maior_indice(dados,"rendimento"))
print(criar_tabela_menor_indice(dados,"rendimento"))
resultado_rendimento <- criar_tabela_comparativa(dados, "rendimento")
print(resultado_rendimento)
print(gerar_grafico_barras(dados,"rendimento"))


#Quartos
grafico_quartos<-ggplot(dados,aes(x=reorder(país,quartos), y =quartos)) +
  geom_point(aes(y=max(quartos,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(quartos,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(quartos,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=quartos,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(quartos, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(quartos, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição do número médio de quartos por pessoa em alojamentos ocupados",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Número médio de Quartos"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_quartos)
print(criar_tabela_maior_indice(dados,"quartos"))
print(criar_tabela_menor_indice(dados,"quartos"))
resultado_quartos <- criar_tabela_comparativa(dados, "quartos")
print(resultado_quartos)
print(gerar_grafico_barras(dados,"quartos"))


#Vida
grafico_vida<-ggplot(dados,aes(x=reorder(país,vida), y =vida)) +
  geom_point(aes(y=max(vida,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(vida,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(vida,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=vida,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(vida, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(vida, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da esperança média de vida à nascença",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Esperança média de vida"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_vida)
print(criar_tabela_maior_indice(dados,"vida"))
print(criar_tabela_menor_indice(dados,"vida"))
resultado_vida <- criar_tabela_comparativa(dados, "vida")
print(resultado_vida)
print(gerar_grafico_barras(dados,"vida"))



#Mortalidade
grafico_mortalidade<-ggplot(dados,aes(x=reorder(país,mortalidade), y =mortalidade)) +
  geom_point(aes(y=max(mortalidade,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(mortalidade,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(mortalidade,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=mortalidade,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(mortalidade, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(mortalidade, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da taxa de mortalidade padronizada (por 1000 habitantes)",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Taxa de Mortalidade (por 1000 habitantes)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_mortalidade)
print(criar_tabela_maior_indice(dados,"mortalidade"))
print(criar_tabela_menor_indice(dados,"mortalidade"))
resultado_mortalidade <- criar_tabela_comparativa(dados, "mortalidade")
print(resultado_mortalidade)
print(gerar_grafico_barras(dados,"mortalidade"))


#Poluição

grafico_poluicao<-ggplot(dados,aes(x=reorder(país,poluição), y =poluição)) +
  geom_point(aes(y=max(poluição,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(poluição,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(poluição,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=poluição,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(poluição, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(poluição, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição do nível de Poluição do ar de PM2.5 (microgramas por metro cúbico)",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Nível de Poluição do ar de PM2.5 (microgramas por metro cúbico)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_poluicao)
print(criar_tabela_maior_indice(dados,"poluição"))
print(criar_tabela_menor_indice(dados,"poluição"))
resultado_poluicao <- criar_tabela_comparativa(dados, "poluição")
print(resultado_poluicao)
print(gerar_grafico_barras(dados,"poluição"))



#Homicidios
grafico_homicidios<-ggplot(dados,aes(x=reorder(país,homicidios), y =poluição)) +
  geom_point(aes(y=max(homicidios,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(homicidios,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(homicidios,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=homicidios,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(homicidios, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(homicidios, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da taxa média de Homicídios (por 100000 habitantes)",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Taxa de Homicídios (por 100000 haitantes)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_homicidios)
print(criar_tabela_maior_indice(dados,"homicidios"))
print(criar_tabela_menor_indice(dados,"homicidios"))
resultado_homicidios <- criar_tabela_comparativa(dados, "homicidios")
print(resultado_homicidios)
print(gerar_grafico_barras(dados,"homicidios"))


#Votos
grafico_voto<-ggplot(dados,aes(x=reorder(país,voto), y =voto)) +
  geom_point(aes(y=max(voto,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(voto,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(voto,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=voto,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(voto, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(voto, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição do Comparecimento em eleições gerais ",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Comparecimento eleitoral em eleições gerais (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_voto)
print(criar_tabela_maior_indice(dados,"voto"))
print(criar_tabela_menor_indice(dados,"voto"))
resultado_votos <- criar_tabela_comparativa(dados, "voto")
print(resultado_votos)
print(gerar_grafico_barras(dados,"voto"))



#Internet
grafico_internet<-ggplot(dados,aes(x=reorder(país,internet), y =internet)) +
  geom_point(aes(y=max(internet,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(internet,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(internet,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=internet,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(internet, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(internet, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da Percentagem de agregados familiares com acesso à internet (em banda larga)",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Agregados familiares com acesso à Internet (em Banda Larga)(%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_internet)
print(criar_tabela_maior_indice(dados,"internet"))
print(criar_tabela_menor_indice(dados,"internet"))
resultado_internet<-criar_tabela_comparativa(dados, "internet")
print(resultado_internet)
print(gerar_grafico_barras(dados,"internet"))



#Social
grafico_social<-ggplot(dados,aes(x=reorder(país,social), y =social)) +
  geom_point(aes(y=max(social,na.rm=TRUE),color = "Máximo"), size = 2) +
  geom_point(aes(y=min(social,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(social,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=social,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(social, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(social, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição da Rede de Suporte Social",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Percentagem de pessoas que têm uma rede de suporte social (%)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_social)
print(criar_tabela_maior_indice(dados,"social"))
print(criar_tabela_menor_indice(dados,"social"))
resultado_social<-criar_tabela_comparativa(dados, "social")
print(resultado_social)
print(gerar_grafico_barras(dados,"social"))


#satisfação
grafico_satisfação<-ggplot(dados,aes(x=reorder(país,satisfação), y =satisfação)) +
  geom_point(aes(y=max(satisfação,na.rm=TRUE),color = "Máximo"), size = 2)+
  geom_point(aes(y=min(satisfação,na.rm=TRUE),color = "Mínimo"), size = 2) +
  geom_line(aes(y=mean(satisfação,na.rm=TRUE),group = 1, color = "Média geral"), size = 1.2) +
  geom_point(aes(y=satisfação,color = "Valores individuais"), size = 2) +
  geom_line(
    data =dados %>% group_by(país) %>% summarise(media = mean(satisfação, na.rm = TRUE)),
    aes(x = reorder(país, media), y = media, group = 1, color = "Média por país"),
    size = 1.2
  ) +
  geom_point(
    data=dados%>% group_by(país) %>% summarise(media = mean(satisfação, na.rm = TRUE)),
    aes(x=reorder(país,media),y =media, color="Média por país"),
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Máximo"="green",
      "Mínimo"="red",
      "Média geral"="yellow",
      "Valores individuais"="magenta",
      "Média por país"="blue"
    ),
    name="Legenda"
  ) +
  labs(
    title ="Distribuição do Índice de Satisfação com a Vida",
    subtitle="Com ligação entre os valores médios de cada país",
    x ="País",
    y="Índice de Satisfação com a Vida (0-10)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom" 
  )

print(grafico_satisfação)
print(criar_tabela_maior_indice(dados,"satisfação"))
print(criar_tabela_menor_indice(dados,"satisfação"))
resultado_satisfacao<-criar_tabela_comparativa(dados, "satisfação")
print(resultado_satisfacao)
print(gerar_grafico_barras(dados,"satisfação"))



# Filtrar os dados para manter apenas os países do continente Europe (usando a coluna 'continente')
dados_europe <- dados %>% 
  filter(grepl("Europe", continente, ignore.case = TRUE))

# Carregar os dados geográficos (shapefile) dos países do mundo
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filtrar apenas os países da Europa
europe <- world %>% filter(continent == "Europe")

# Excluir países indesejados (certifique-se de que os nomes estejam corretos)
excluir <- c("Russia", "Ukraine", "Belarus", "Moldova", "Romania", "Croatia", "Serbia", "Bulgaria")
europe <- europe %>% filter(!(name %in% excluir))

# Realizar a junção dos dados
europe_data <- europe %>%
  left_join(dados, by = c("name" = "país"))


# Criar o mapa coroplético para o comparecimento eleitoral
ggplot(europe_data) +
  geom_sf(aes(fill = voto), color = "gray50") +
  scale_fill_viridis(option = "plasma", na.value = "white", name = "Voto") +
  labs(title = "Mapa Coroplético de Comparecimento Eleitoral na Europa") +
  theme_minimal()


