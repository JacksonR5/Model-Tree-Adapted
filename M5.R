#git pull origin main
################################## PACOTES #####################################
library(data.table)
library(caret)
library(dplyr)

#################################### DATA ######################################
data = fread("C:\\Users\\cyber\\OneDrive - UNINTER - Aluno\\OneDrive\\ARTIGO\\PIBIC\\data.csv", stringsAsFactor=TRUE)
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

################### CALCULAR MÁXIMA REDUÇÃO DE VARIÂNCIA #######################
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

# Exibir detalhes da melhor partição para variável D
if (!is.null(indice_melhor_particao_D)) {
  cat("A partição com a maior redução de variância para variável D é a Partição", indice_melhor_particao_D - 1, "\n")
  cat("Maior Redução de Variância para variável D:", melhor_reducao_D, "\n")
  cat("Detalhes da Partição:\n")
  print(melhor_particao_objeto_D)
}

# Exibir detalhes da melhor partição para variável H
if (!is.null(indice_melhor_particao_H)) {
  cat("A partição com a maior redução de variância para variável H é a Partição", indice_melhor_particao_H - 1, "\n")
  cat("Maior Redução de Variância para variável H:", melhor_reducao_H, "\n")
  cat("Detalhes da Partição:\n")
  print(melhor_particao_objeto_H)
}

########################## NÓ INTERNO E NÓ TERMINAL ############################
melhor_particao = NULL

if (melhor_reducao_D > melhor_reducao_H) {
  cat("A melhor partição é a D\n")
  melhor_particao = melhor_particao_objeto_D
} else {
  cat("A melhor partição é a H\n")
  melhor_particao = melhor_particao_objeto_H
}

melhor_particao
#REGRESSÃO NÓ TERMINAL DERIVADO DO NÓ-RAIZ
no_terminal_raiz = melhor_particao$subconjunto2

regre_no_terminal_raiz = lm(log(V) ~ log(D) + log(H), data = no_terminal_raiz)
anova(regre_no_terminal_raiz)
summary(regre_no_terminal_raiz)

############ CRESCIMENTO RECURSIVO A PARTIR DE NÓ-INTERNO RAIZ #################

no_interno_raiz = melhor_particao$subconjunto1
CONJUNTO1.1_ORDENADO_PARA_D = no_interno_raiz %>% arrange(desc(D))
CONJUNTO1.1_ORDENADO_PARA_H = no_interno_raiz %>% arrange(desc(H))

# Gerar partições para variável D e H
particoes_D = FUNCTION_PARTICOES(CONJUNTO1.1_ORDENADO_PARA_D, "D")
particoes_H = FUNCTION_PARTICOES(CONJUNTO1.1_ORDENADO_PARA_H, "H")

############### MELHORES PARTIÇÕES A PARTIR DO NÓ-INTERNO RAIZ  ################
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

# Exibir detalhes da melhor partição para variável D
if (!is.null(indice_melhor_particao_D)) {
  cat("A partição com a maior redução de variância para variável D é a Partição", indice_melhor_particao_D - 1, "\n")
  cat("Maior Redução de Variância para variável D:", melhor_reducao_D, "\n")
  cat("Detalhes da Partição:\n")
  print(melhor_particao_objeto_D)
}

# Exibir detalhes da melhor partição para variável H
if (!is.null(indice_melhor_particao_H)) {
  cat("A partição com a maior redução de variância para variável H é a Partição", indice_melhor_particao_H - 1, "\n")
  cat("Maior Redução de Variância para variável H:", melhor_reducao_H, "\n")
  cat("Detalhes da Partição:\n")
  print(melhor_particao_objeto_H)
}
############ NÓ INTERNO E NÓ TERMINAL DERIVADO DO NÓ INTERNO 1 #################
melhor_particao1.1 = NULL

if (melhor_reducao_D > melhor_reducao_H) {
  cat("A melhor partição é a D\n")
  melhor_particao1.1 = melhor_particao_objeto_D
} else {
  cat("A melhor partição é a H\n")
  melhor_particao1.1 = melhor_particao_objeto_H
}

melhor_particao1.1

##################### NÓ TERMINAL DERIVADO DO NÓ INTERNO 1.1 ######################
no_terminal_1.1 = melhor_particao1.1$subconjunto2

regre_no_terminal_1.1 = lm(log(V) ~ log(D) + log(H), data = no_terminal_1.1)
anova(regre_no_terminal_1.1)
summary(regre_no_terminal_1.1)

############ CRESCIMENTO RECURSIVO A PARTIR DE NÓ INTERNO 1.1 #################
