#git pull origin main
################################## PACOTES #####################################
library(data.table)
library(caret)
library(dplyr)

#################################### DATA ######################################
#data = fread("C:\\Users\\cyber\\OneDrive - UNINTER - Aluno\\OneDrive\\ARTIGO\\PIBIC\\data.csv", stringsAsFactor=TRUE)
data = data
data$D = data$D / 100 # Convertendo centímetros para metros
# DIVIDIR TREINO E TESTE
set.seed(100)
trainIndex = createDataPartition(y = data$V, p = 0.70, list = FALSE)
trainingSet = data[trainIndex, ]
testSet = data[-trainIndex, ]

######################## NÓ RAIZ PARA CADA VARIÁVEL INDEPENDENTE ###############
CONJUNTO1_ORDENADO_PARA_D = trainingSet %>% arrange(desc(D))
CONJUNTO1_ORDENADO_PARA_H = trainingSet %>% arrange(desc(H))

############# GERAR TODAS PARTIÇÕES POSSÍVEIS A PARTIR DO NÓ RAÍZ ##############

FUNCTION_PARTICOES = function(CONJUNTO_ORDENADO, var_independente) {
  PARTICOES = list()
  num_max_linhas = nrow(CONJUNTO_ORDENADO)
  
  for (i in 0:num_max_linhas) {
    subconjunto1 = CONJUNTO_ORDENADO[1:i, ]
    subconjunto2 = CONJUNTO_ORDENADO[(i + 1):num_max_linhas, ]
    
    particao = list(subconjunto1 = subconjunto1, subconjunto2 = subconjunto2)
    PARTICOES[[length(PARTICOES) + 1]] = particao
  }
  
  return(PARTICOES)
}

particoes_D = FUNCTION_PARTICOES(CONJUNTO1_ORDENADO_PARA_D, "D")
particoes_H = FUNCTION_PARTICOES(CONJUNTO1_ORDENADO_PARA_H, "H")

#CALCULAR MÁXIMA REDUÇÃO DE VARIÂNCIA PARA TODAS PARTIÇÕES DE CADA VARIÁVEL INDEPENDENTE############################

calcular_maxima_reducao_variancia = function(subconjunto1, subconjunto2) {
  variância_total = var(trainingSet$V)
  proporcao_subconjunto1 = nrow(subconjunto1) / nrow(trainingSet)
  proporcao_subconjunto2 = nrow(subconjunto2) / nrow(trainingSet)
  variancia_subconjunto1_D = var(subconjunto1$D)
  variancia_subconjunto2_D = var(subconjunto2$D)
  variancia_subconjunto1_H = var(subconjunto1$H)
  variancia_subconjunto2_H = var(subconjunto2$H)
  
  maxima_reducao_variancia_D = variância_total - (proporcao_subconjunto1 * variancia_subconjunto1_D +
                                                  proporcao_subconjunto2 * variancia_subconjunto2_D)
  
  maxima_reducao_variancia_H = variância_total - (proporcao_subconjunto1 * variancia_subconjunto1_H +
                                                  proporcao_subconjunto2 * variancia_subconjunto2_H)
  
  return(list(maxima_reducao_variancia_D, maxima_reducao_variancia_H))
}

######################## MELHORES PARTIÇÕES PARA D E H #########################
avaliar_particoes = function(particoes, var_independente) {
  melhor_reducao = -Inf
  indice_melhor_particao = NULL
  melhor_particao = NULL
  
  for (i in 1:length(particoes)) {
    particao = particoes[[i]]
    reducao_variancia_particao = calcular_maxima_reducao_variancia(particao$subconjunto1, particao$subconjunto2)
    
    if (!any(is.na(reducao_variancia_particao)) && reducao_variancia_particao[[1]] > melhor_reducao) {
      melhor_reducao = reducao_variancia_particao[[1]]
      indice_melhor_particao = i
      melhor_particao = particao
    }
  }
  
  return(list(melhor_reducao, indice_melhor_particao, melhor_particao))
}

# Avaliar e obter a melhor partição para variável D
melhor_particao_D = avaliar_particoes(particoes_D, "D")
melhor_reducao_D = melhor_particao_D[[1]]
indice_melhor_particao_D = melhor_particao_D[[2]]
melhor_particao_objeto_D = melhor_particao_D[[3]]

# Avaliar e obter a melhor partição para variável H
melhor_particao_H = avaliar_particoes(particoes_H, "H")
melhor_reducao_H = melhor_particao_H[[1]]
indice_melhor_particao_H = melhor_particao_H[[2]]
melhor_particao_objeto_H = melhor_particao_H[[3]]

# Função para exibir detalhes da partição
exibir_detalhes_particao <- function(variavel, reducao, indice_melhor_particao, melhor_particao) {
  cat("A melhor partição para variável", variavel, "tem uma redução de variância de", reducao, "\n")
  cat("Partição:", indice_melhor_particao - 1, "\n")
  cat("Detalhes da Partição:\n")
  print.data.frame(melhor_particao$subconjunto1, row.names = FALSE)
  cat("\n")
  print.data.frame(melhor_particao$subconjunto2, row.names = FALSE)
  cat("\n")
}

# Exibir detalhes da melhor partição para variável D
if (!is.null(indice_melhor_particao_D)) {
  exibir_detalhes_particao("D", melhor_reducao_D, indice_melhor_particao_D, melhor_particao_objeto_D)
}

# Exibir detalhes da melhor partição para variável H
if (!is.null(indice_melhor_particao_H)) {
  exibir_detalhes_particao("H", melhor_reducao_H, indice_melhor_particao_H, melhor_particao_objeto_H)
}



################################ MELHOR PARTIÇÃO ###############################
melhor_particao = NULL

if (melhor_reducao_D > melhor_reducao_H) {
  cat("A melhor partição é a D\n")
  melhor_particao = melhor_particao_objeto_D
} else {
  cat("A melhor partição é a H\n")
  melhor_particao = melhor_particao_objeto_H
}


############## REGRESSÃO NO NÓ TERMINAL DERIVADO DO NÓ-RAIZ ####################
no_terminal_raiz = melhor_particao$subconjunto2

regre_no_terminal_raiz = lm(log(V) ~ log(D) + log(H), data = no_terminal_raiz)
anova(regre_no_terminal_raiz)
summary(regre_no_terminal_raiz)

###### CRESCIMENTO RECURSIVO A PARTIR DE NÓ-INTERNO DERIVADO DO NÓ RAIZ ########
no_interno_raiz = melhor_particao$subconjunto1
no_interno_raiz

CONJUNTO1.1_ORDENADO_PARA_D = no_interno_raiz %>% arrange(desc(D))
CONJUNTO1.1_ORDENADO_PARA_H = no_interno_raiz %>% arrange(desc(H))

particoes_D1.1 = FUNCTION_PARTICOES(CONJUNTO1.1_ORDENADO_PARA_D, "D")
particoes_H1.1 = FUNCTION_PARTICOES(CONJUNTO1.1_ORDENADO_PARA_H, "H")

# Função para exibir detalhes da partição
exibir_detalhes_particao <- function(variavel, reducao, indice_melhor_particao, melhor_particao) {
  cat("A partição com a maior redução de variância para variável", variavel, "é a Partição", indice_melhor_particao - 1, "\n")
  cat("Maior Redução de Variância para variável", variavel, ":", reducao, "\n")
  cat("Detalhes da Partição:\n")
  print.data.frame(melhor_particao$subconjunto1, row.names = FALSE)
  cat("\n")
  print.data.frame(melhor_particao$subconjunto2, row.names = FALSE)
  cat("\n")
}

# Avaliar e obter a melhor partição para variável D
melhor_particao_D1.1 = avaliar_particoes(particoes_D1.1, "D")
melhor_reducao_D1.1 = melhor_particao_D1.1[[1]]
indice_melhor_particao_D1.1 = melhor_particao_D1.1[[2]]
melhor_particao_objeto_D1.1 = melhor_particao_D1.1[[3]]

# Avaliar e obter a melhor partição para variável H
melhor_particao_H1.1 = avaliar_particoes(particoes_H1.1, "H")
melhor_reducao_H1.1 = melhor_particao_H1.1[[1]]
indice_melhor_particao_H1.1 = melhor_particao_H1.1[[2]]
melhor_particao_objeto_H1.1 = melhor_particao_H1.1[[3]]

# Exibir detalhes da melhor partição para variável D
if (!is.null(indice_melhor_particao_D1.1)) {
  exibir_detalhes_particao("D", melhor_reducao_D1.1, indice_melhor_particao_D1.1, melhor_particao_objeto_D1.1)
}

# Exibir detalhes da melhor partição para variável H
if (!is.null(indice_melhor_particao_H1.1)) {
  exibir_detalhes_particao("H", melhor_reducao_H1.1, indice_melhor_particao_H1.1, melhor_particao_objeto_H1.1)
}

# Determinar a melhor partição no nó interno 1.1
melhor_particao1.1 = NULL

if (melhor_reducao_D1.1 > melhor_reducao_H1.1) {
  cat("A melhor partição fica na variável independente D\n")
  melhor_particao1.1 = melhor_particao_objeto_D1.1
} else {
  cat("A melhor partição fica na variável independente H\n")
  melhor_particao1.1 = melhor_particao_objeto_H1.1
}

##################### NÓ TERMINAL1.1.1 DERIVADO DO NÓ INTERNO 1.1 ###################
no_terminal_1.1.1 = melhor_particao1.1$subconjunto2
no_terminal_1.1.1
########### REGRESSÃO NO NÓ TERMINAL DERIVADO DO NÓ INTERNO 1.1 ################
regre_no_terminal_1.1.1 = lm(log(V) ~ log(D) + log(H), data = no_terminal_1.1.1)
anova(regre_no_terminal_1.1.1)
summary(regre_no_terminal_1.1.1)

############ NÓ TERMINAL1.1.2 DERIVADO DO NÓ INTERNO 1.1 #################
no_interno_1.1.2 = melhor_particao1.1$subconjunto1
no_interno_1.1.2
########### REGRESSÃO NO NÓ TERMINAL DERIVADO DO NÓ INTERNO 1.1 ################
regre_no_terminal_1.1.2 = lm(log(V) ~ log(D) + log(H), data = no_interno_1.1.2)
anova(regre_no_terminal_1.1.2)
summary(regre_no_terminal_1.1.2)


############################## PREDIÇÃO ########################################

# Para cada variável dependente no conjunto testSet, realize previsões
predicted_values_raiz <- c()  # Armazenar as previsões do Nó Terminal Raiz
predicted_values_1.1 <- c()    # Armazenar as previsões do Nó Terminal 1.1
predicted_values_1.2 <- c()    # Armazenar as previsões do Nó Terminal 1.2

for (i in 1:nrow(testSet)) {
  row <- testSet[i, ]
  
  # Previsão a partir do Nó Terminal Raiz
  predicted_value_raiz <- exp(predict(regre_no_terminal_raiz, newdata = row))
  predicted_values_raiz <- c(predicted_values_raiz, predicted_value_raiz)
  
  # Verificar qual nó terminal usar para o Nó Interno 1.1
  if (row$D > melhor_particao1.1$subconjunto1$D[1]) {
    # Previsão a partir do Nó Terminal 1.1
    predicted_value_1.1 <- exp(predict(regre_no_terminal_1.1, newdata = row))
    predicted_values_1.1 <- c(predicted_values_1.1, predicted_value_1.1)
    
    # Verificar qual nó terminal usar para o Nó Interno 1.2
    if (row$H > melhor_particao1.1$subconjunto2$H[1]) {
      # Previsão a partir do Nó Terminal 1.2
      predicted_value_1.2 <- exp(predict(regre_no_terminal_1.2, newdata = row))
      predicted_values_1.2 <- c(predicted_values_1.2, predicted_value_1.2)
    } else {
      # Previsão a partir do Nó Terminal 1.3 (caso Nó Interno 1.2 não seja selecionado)
      predicted_value_1.3 <- exp(predict(regre_no_terminal_1.3, newdata = row))
      predicted_values_1.2 <- c(predicted_values_1.2, predicted_value_1.3)
    }
  }
}

# Calcular R² para as previsões do Nó Terminal Raiz
actual_values <- testSet$V
rss_raiz <- sum((actual_values - predicted_values_raiz)^2)
tss <- sum((actual_values - mean(actual_values))^2)
rsquared_raiz <- 1 - (rss_raiz / tss)
cat("R² for Nó Terminal Raiz:", rsquared_raiz, "\n")

# Calcular RMSE para as previsões do Nó Terminal Raiz
rmse_raiz <- sqrt(mean((actual_values - predicted_values_raiz)^2))
cat("RMSE for Nó Terminal Raiz:", rmse_raiz, "\n")

# Calcular R² para as previsões do Nó Terminal 1.1 (se aplicável)
if (length(predicted_values_1.1) > 0) {
  rss_1.1 <- sum((actual_values - predicted_values_1.1)^2)
  rsquared_1.1 <- 1 - (rss_1.1 / tss)
  cat("R² for Nó Terminal 1.1:", rsquared_1.1, "\n")
  
  # Calcular RMSE para as previsões do Nó Terminal 1.1
  rmse_1.1 <- sqrt(mean((actual_values - predicted_values_1.1)^2))
  cat("RMSE for Nó Terminal 1.1:", rmse_1.1, "\n")
}

# Calcular R² para as previsões do Nó Terminal 1.2 (se aplicável)
if (length(predicted_values_1.2) > 0) {
  rss_1.2 <- sum((actual_values - predicted_values_1.2)^2)
  rsquared_1.2 <- 1 - (rss_1.2 / tss)
  cat("R² for Nó Terminal 1.2:", rsquared_1.2, "\n")
  
  # Calcular RMSE para as previsões do Nó Terminal 1.2
  rmse_1.2 <- sqrt(mean((actual_values - predicted_values_1.2)^2))
  cat("RMSE for Nó Terminal 1.2:", rmse_1.2, "\n")
}