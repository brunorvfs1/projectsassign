# Setup ----------------------------------
# Limpando o ambiente 

rm(list = ls())

# Definindo o diretório (cada um coloque o seu)

setwd("Desktop/FGV EESP/Data Science")

# Carregando pacotes

load.lib <- c("dplyr", "readr", "stargazer",
              "ggplot2", "knitr", "tidyr")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

# Dados -----------------------------------

# Importando as bases de dados

# Setup ----------------------------------
# Limpando o ambiente 

rm(list = ls())

# Definindo o diretório (cada um coloque o seu)

setwd("Desktop/FGV EESP/Data Science")

# Carregando pacotes

load.lib <- c("dplyr", "readr", "stargazer",
              "ggplot2", "knitr", "tidyr")

install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib,require,character=TRUE)

# Dados -----------------------------------

# Importando as bases de dados

df_1 <- read_csv("data_1_grupo_8.csv")
df_2 <- read_csv("data_2_grupo_8.csv")

### 1) identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base1 <- function(colunas) {
  conjunto_atual <- df_1[, colunas]
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}

# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base1(conjunto)
}

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos <- list()

# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_1[, conjunto]
  dataframes_conjuntos[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1 <- dataframes_conjuntos$conjunto_2
conj3_base1 <- dataframes_conjuntos$conjunto_3
conj4_base1 <- dataframes_conjuntos$conjunto_4
conj5_base1 <- dataframes_conjuntos$conjunto_5
conj6_base1 <- dataframes_conjuntos$conjunto_6

# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("No NAs found in", nome_dataframe, "\n")
  } else {
    cat("NAs found in", nome_dataframe, "at the following rows and columns:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Row:", linha, "Column:", coluna, "\n")
    }
  }
}


### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b1 <- which(df_1[, 31:60] < 0 | df_1[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b1)) {
    linha <- erros_c2_b1[i, 1]
    coluna <- 30 + erros_c2_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b1 <- which(df_1[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b1)) {
    linha <- erros_c3_b1[i, 1]
    coluna <- 60 + erros_c3_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b1 <- which(df_1[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b1)) {
    linha <- erros_c4_b1[i, 1]
    coluna <- 90 + erros_c4_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_1[, 121:150] < 0 | df_1[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


## Dentro de cada novo Dataframe:

# Função para identificar valores fora do intervalo
identificar_valores_fora_intervalo <- function(dataframe, limite_inferior, limite_superior) {
  erros <- which(dataframe < limite_inferior | dataframe > limite_superior, arr.ind = TRUE)
  return(erros)
}

# Definir limites de intervalo para cada conjunto
limites_conj2 <- c(0, 1)
limites_conj3 <- c(0, Inf)
limites_conj4 <- c(-Inf, 0)
limites_conj5 <- c(0, 300)

# Identificar valores fora do intervalo para cada conjunto
erros_conj2 <- identificar_valores_fora_intervalo(conj2_base1, limites_conj2[1], limites_conj2[2])
erros_conj3 <- identificar_valores_fora_intervalo(conj3_base1, limites_conj3[1], limites_conj3[2])
erros_conj4 <- identificar_valores_fora_intervalo(conj4_base1, limites_conj4[1], limites_conj4[2])
erros_conj5 <- identificar_valores_fora_intervalo(conj5_base1, limites_conj5[1], limites_conj5[2])

# Exibir os erros para cada conjunto
if (nrow(erros_conj2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2)) {
    linha <- erros_conj2[i, 1]
    coluna <- erros_conj2[i, 2]
    valor <- conj2_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj3) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3)) {
    linha <- erros_conj3[i, 1]
    coluna <- erros_conj3[i, 2]
    valor <- conj3_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj4) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4)) {
    linha <- erros_conj4[i, 1]
    coluna <- erros_conj4[i, 2]
    valor <- conj4_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj5) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 5.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 5 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5)) {
    linha <- erros_conj5[i, 1]
    coluna <- erros_conj5[i, 2]
    valor <- conj5_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}



###################### BOX PLOTS #####################################

#acessar um dos dataframes resultantes 
conj1_base1 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base1<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base1<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base1 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base1<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base1<-dataframes_conjuntos$conjunto_6 %>% 
  gather()


#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base1$key, levels = variaveis)

ggplot(data = conj1_base1, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base1$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base1[1:8500,], aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base1$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base1$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")


##ESTATÍSTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1<- dataframes_conjuntos$conjunto_2
conj3_base1<-dataframes_conjuntos$conjunto_3 
conj4_base1 <-dataframes_conjuntos$conjunto_4
conj5_base1<-dataframes_conjuntos$conjunto_5 
conj6_base1<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados <- list(
  conj1_base1,
  conj2_base1,
  conj3_base1,
  conj4_base1,
  conj5_base1,
  conj6_base1
)

# Lista para armazenar os resumos estatísticos
resumos <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados) {
  resumos[[length(resumos) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana

resumos


# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código

# Definindo um dataframe com os limites de cada variável

limits <- as.data.frame(matrix(NA, nrow = 2, ncol = 250))

for(i in 1:30) {
  limits[[i]] <- c(-Inf, Inf)
  limits[[i+30]] <- c(0, 1)
  limits[[i+60]] <- c(0, Inf)
  limits[[i+90]] <- c(-Inf, 0)
  limits[[i+120]] <- c(0, 300)
}

for(i in 151:250) {
  limits[[i]] <- c(-Inf, Inf)
}

# Iterar pelas colunas de df_1 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_1[, col][df_1[, col] < min_limit | 
                df_1[, col] > max_limit] <- NA
}

# Removendo NA's
df_1 <- na.omit(df_1)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_1_standard <- as.data.frame(scale(df_1))

###########REGRESSÃO E SCATTLEPLOT-BASE 1###############
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_1_reg <-df_1_standard[, c("y", paste0("V", 1:205))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg1 <- lm(y ~ ., data = df_1_reg)
summary(reg1)
stargazer(reg1, type = "text")
reg2=lm(y~., data=df_1_standard)
summary(reg2)
stargazer(reg1, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_1_standard$y,df_1_standard$V1,xlab = "V1", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V56
plot(df_1_standard$y,df_1_standard$V56,xlab = "V56", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V112
plot(df_1_standard$y,df_1_standard$V112, xlab = "V112", ylab = "Y")
abline(reg1)

#Gráfco da regressão com todas as variáveis
plot(df_1_standard$y,df_1_standard$V1, xlab = "V1", ylab = "Y")
abline(reg2)

(AQUI COLOCAR O RESTO DO CODIGO PRA BASE 1)











#### - BASE DE DADOS 2----------------------------------------------------------

#### - BASE DE DADOS 2----------------------------------------------------------

### 1) Identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base2 <- function(colunas) {
  # Selecionar apenas as colunas do conjunto atual
  conjunto_atual <- df_2[, colunas]
  
  # Identificar valores NA no conjunto atual
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  # Exibir resultados para o conjunto atual
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}


# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base2(conjunto)
}

# Criando data frames para cada conjunto da base 2

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos2 <- list()


# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_2[, conjunto]
  dataframes_conjuntos2[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base2 <- dataframes_conjuntos2$conjunto_1
conj2_base2 <- dataframes_conjuntos2$conjunto_2
conj3_base2 <- dataframes_conjuntos2$conjunto_3
conj4_base2 <- dataframes_conjuntos2$conjunto_4
conj5_base2 <- dataframes_conjuntos2$conjunto_5
conj6_base2 <- dataframes_conjuntos2$conjunto_6


# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na2 <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("Nenhum NA encontrado em", nome_dataframe, "\n")
  } else {
    cat("NAs encontrados em", nome_dataframe, "nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", coluna, "\n")
    }
  }
}
### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b2 <- which(df_2[, 31:60] < 0 | df_2[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60 no Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b2)) {
    linha <- erros_c2_b2[i, 1]
    coluna <- 30 + erros_c2_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}




# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b2 <- which(df_2[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b2)) {
    linha <- erros_c3_b2[i, 1]
    coluna <- 60 + erros_c3_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b2 <- which(df_2[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b2)) {
    linha <- erros_c4_b2[i, 1]
    coluna <- 90 + erros_c4_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_2[, 121:150] < 0 | df_2[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}

## Dentro de cada novo Dataframe:

# Identificar valores fora do intervalo para cada conjunto
erros_conj2_2 <- identificar_valores_fora_intervalo(conj2_base2, limites_conj2[1], limites_conj2[2])
erros_conj3_2<- identificar_valores_fora_intervalo(conj3_base2, limites_conj3[1], limites_conj3[2])
erros_conj4_2 <- identificar_valores_fora_intervalo(conj4_base2, limites_conj4[1], limites_conj4[2])
erros_conj5_2 <- identificar_valores_fora_intervalo(conj5_base2, limites_conj5[1], limites_conj5[2])


# Exibir os erros para cada conjunto

#Conjunto 1

if (nrow(erros_conj2_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2_2)) {
    linha <- erros_conj2_2[i, 1]
    coluna <- erros_conj2_2[i, 2]
    valor <- conj2_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


#Conjunto 3
if (nrow(erros_conj3_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3_2)) {
    linha <- erros_conj3_2[i, 1]
    coluna <- erros_conj3_2[i, 2]
    valor <- conj3_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto 4
if (nrow(erros_conj4_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4_2)) {
    linha <- erros_conj4_2[i, 1]
    coluna <- erros_conj4_2[i, 2]
    valor <- conj4_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto54
if (nrow(erros_conj5_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5_2)) {
    linha <- erros_conj5_2[i, 1]
    coluna <- erros_conj5_2[i, 2]
    valor <- conj5_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

###################### BOX PLOTS 2 #####################################

#acessar um dos dataframes resultantes 
conj1_base2 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base2<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base2<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base2 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base2<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base2<-dataframes_conjuntos$conjunto_6 %>% 
  gather()

#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base2$key, levels = variaveis)

ggplot(data = conj1_base2, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 3") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 4") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 5") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base2$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.1, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base2$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base2$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

##ESTATISTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base2 <- dataframes_conjuntos$conjunto_1
conj2_base2<- dataframes_conjuntos$conjunto_2
conj3_base2<-dataframes_conjuntos$conjunto_3 
conj4_base2 <-dataframes_conjuntos$conjunto_4
conj5_base2<-dataframes_conjuntos$conjunto_5 
conj6_base2<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados.2 <- list(
  conj1_base2,
  conj2_base2,
  conj3_base2,
  conj4_base2,
  conj5_base2,
  conj6_base2
)

# Lista para armazenar os resumos estatísticos
resumos.2 <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados.2) {
  resumos.2[[length(resumos.2) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana
resumos.2

##################################################

# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código
# Iterar pelas colunas de df_2 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_2[, col][df_2[, col] < min_limit | 
                df_2[, col] > max_limit] <- NA
}

# Removendo NA's
df_2 <- na.omit(df_2)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_2_standard <- as.data.frame(scale(df_2))

#######REGRESSÃO E SCATTLEPLOT-BASE 2####################
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_2_reg <-df_2_standard[, c("y", paste0("V", 1:200))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg3 <- lm(y ~ ., data = df_2_reg)
summary(reg3)
stargazer(reg3, type = "text")
reg4=lm(y~., data=df_2_standard)
summary(reg4)
stargazer(reg4, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_2_standard$y,df_2_standard$V1, xlab = "V1", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V56
plot(df_2_standard$y,df_2_standard$V57, xlab = "V57", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V112
plot(df_2_standard$y,df_2_standard$V112,xlab = "V112", ylab = "Y")
abline(reg1)

#Regressão com todas as variáveis(Como não possível testar a significância, foi considerado apenas a variável V1)
plot(df_2_standard$y,df_2_standard$V1,xlab = "V1", ylab = "Y")
abline(reg4)




TAREFA 2



#################

#TAREFA 2 - Modelos Lineares Esparsos ou Densos
# GRUPO 8
# Outubro, 2023

#################

# 0. Set-up
# =================================================================

rm(list=ls())

# pacotes

packages<-c("dplyr", "readr", "stargazer",
            "ggplot2", "knitr", "tidyr", "tinytex",
            "rmarkdown","caTools")
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


# DIRETÓRIO
# ------------------------------------


setwd("C:/Users/nfrig/OneDrive/Desktop/trabalhoDS")

#--------------- CODIGO REFERENTE à TAREFA 1 (Pré-Processamento)-----------------


# Importando as bases de dados

df_1 <- read_csv("data_1_grupo_8.csv")
df_2 <- read_csv("data_2_grupo_8.csv")

### 1) identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base1 <- function(colunas) {
  conjunto_atual <- df_1[, colunas]
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}

# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base1(conjunto)
}

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos <- list()

# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_1[, conjunto]
  dataframes_conjuntos[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1 <- dataframes_conjuntos$conjunto_2
conj3_base1 <- dataframes_conjuntos$conjunto_3
conj4_base1 <- dataframes_conjuntos$conjunto_4
conj5_base1 <- dataframes_conjuntos$conjunto_5
conj6_base1 <- dataframes_conjuntos$conjunto_6

# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("No NAs found in", nome_dataframe, "\n")
  } else {
    cat("NAs found in", nome_dataframe, "at the following rows and columns:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Row:", linha, "Column:", coluna, "\n")
    }
  }
}


### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b1 <- which(df_1[, 31:60] < 0 | df_1[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b1)) {
    linha <- erros_c2_b1[i, 1]
    coluna <- 30 + erros_c2_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b1 <- which(df_1[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b1)) {
    linha <- erros_c3_b1[i, 1]
    coluna <- 60 + erros_c3_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b1 <- which(df_1[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b1)) {
    linha <- erros_c4_b1[i, 1]
    coluna <- 90 + erros_c4_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_1[, 121:150] < 0 | df_1[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


## Dentro de cada novo Dataframe:

# Função para identificar valores fora do intervalo
identificar_valores_fora_intervalo <- function(dataframe, limite_inferior, limite_superior) {
  erros <- which(dataframe < limite_inferior | dataframe > limite_superior, arr.ind = TRUE)
  return(erros)
}

# Definir limites de intervalo para cada conjunto
limites_conj2 <- c(0, 1)
limites_conj3 <- c(0, Inf)
limites_conj4 <- c(-Inf, 0)
limites_conj5 <- c(0, 300)

# Identificar valores fora do intervalo para cada conjunto
erros_conj2 <- identificar_valores_fora_intervalo(conj2_base1, limites_conj2[1], limites_conj2[2])
erros_conj3 <- identificar_valores_fora_intervalo(conj3_base1, limites_conj3[1], limites_conj3[2])
erros_conj4 <- identificar_valores_fora_intervalo(conj4_base1, limites_conj4[1], limites_conj4[2])
erros_conj5 <- identificar_valores_fora_intervalo(conj5_base1, limites_conj5[1], limites_conj5[2])

# Exibir os erros para cada conjunto
if (nrow(erros_conj2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2)) {
    linha <- erros_conj2[i, 1]
    coluna <- erros_conj2[i, 2]
    valor <- conj2_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj3) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3)) {
    linha <- erros_conj3[i, 1]
    coluna <- erros_conj3[i, 2]
    valor <- conj3_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj4) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4)) {
    linha <- erros_conj4[i, 1]
    coluna <- erros_conj4[i, 2]
    valor <- conj4_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj5) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 5.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 5 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5)) {
    linha <- erros_conj5[i, 1]
    coluna <- erros_conj5[i, 2]
    valor <- conj5_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}



###################### BOX PLOTS #####################################

#acessar um dos dataframes resultantes 
conj1_base1 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base1<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base1<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base1 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base1<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base1<-dataframes_conjuntos$conjunto_6 %>% 
  gather()


#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base1$key, levels = variaveis)

ggplot(data = conj1_base1, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base1$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base1[1:8500,], aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base1$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base1$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")


##ESTATÍSTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1<- dataframes_conjuntos$conjunto_2
conj3_base1<-dataframes_conjuntos$conjunto_3 
conj4_base1 <-dataframes_conjuntos$conjunto_4
conj5_base1<-dataframes_conjuntos$conjunto_5 
conj6_base1<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados <- list(
  conj1_base1,
  conj2_base1,
  conj3_base1,
  conj4_base1,
  conj5_base1,
  conj6_base1
)

# Lista para armazenar os resumos estatísticos
resumos <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados) {
  resumos[[length(resumos) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana

resumos


# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código

# Definindo um dataframe com os limites de cada variável

limits <- as.data.frame(matrix(NA, nrow = 2, ncol = 250))

for(i in 1:30) {
  limits[[i]] <- c(-Inf, Inf)
  limits[[i+30]] <- c(0, 1)
  limits[[i+60]] <- c(0, Inf)
  limits[[i+90]] <- c(-Inf, 0)
  limits[[i+120]] <- c(0, 300)
}

for(i in 151:250) {
  limits[[i]] <- c(-Inf, Inf)
}

# Iterar pelas colunas de df_1 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_1[, col][df_1[, col] < min_limit | 
                df_1[, col] > max_limit] <- NA
}

# Removendo NA's
df_1 <- na.omit(df_1)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_1_standard <- as.data.frame(scale(df_1))

###########REGRESSÃO E SCATTLEPLOT-BASE 1###############
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_1_reg <-df_1_standard[, c("y", paste0("V", 1:205))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg1 <- lm(y ~ ., data = df_1_reg)
summary(reg1)
stargazer(reg1, type = "text")
reg2=lm(y~., data=df_1_standard)
summary(reg2)
stargazer(reg1, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_1_standard$y,df_1_standard$V1,xlab = "V1", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V56
plot(df_1_standard$y,df_1_standard$V56,xlab = "V56", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V112
plot(df_1_standard$y,df_1_standard$V112, xlab = "V112", ylab = "Y")
abline(reg1)

#Gráfco da regressão com todas as variáveis
plot(df_1_standard$y,df_1_standard$V1, xlab = "V1", ylab = "Y")
abline(reg2)


#### - BASE DE DADOS 2----------------------------------------------------------

### 1) Identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base2 <- function(colunas) {
  # Selecionar apenas as colunas do conjunto atual
  conjunto_atual <- df_2[, colunas]
  
  # Identificar valores NA no conjunto atual
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  # Exibir resultados para o conjunto atual
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}


# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base2(conjunto)
}

# Criando data frames para cada conjunto da base 2

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos2 <- list()


# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_2[, conjunto]
  dataframes_conjuntos2[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base2 <- dataframes_conjuntos2$conjunto_1
conj2_base2 <- dataframes_conjuntos2$conjunto_2
conj3_base2 <- dataframes_conjuntos2$conjunto_3
conj4_base2 <- dataframes_conjuntos2$conjunto_4
conj5_base2 <- dataframes_conjuntos2$conjunto_5
conj6_base2 <- dataframes_conjuntos2$conjunto_6


# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na2 <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("Nenhum NA encontrado em", nome_dataframe, "\n")
  } else {
    cat("NAs encontrados em", nome_dataframe, "nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", coluna, "\n")
    }
  }
}
### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b2 <- which(df_2[, 31:60] < 0 | df_2[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60 no Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b2)) {
    linha <- erros_c2_b2[i, 1]
    coluna <- 30 + erros_c2_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}




# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b2 <- which(df_2[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b2)) {
    linha <- erros_c3_b2[i, 1]
    coluna <- 60 + erros_c3_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b2 <- which(df_2[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b2)) {
    linha <- erros_c4_b2[i, 1]
    coluna <- 90 + erros_c4_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_2[, 121:150] < 0 | df_2[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}

## Dentro de cada novo Dataframe:

# Identificar valores fora do intervalo para cada conjunto
erros_conj2_2 <- identificar_valores_fora_intervalo(conj2_base2, limites_conj2[1], limites_conj2[2])
erros_conj3_2<- identificar_valores_fora_intervalo(conj3_base2, limites_conj3[1], limites_conj3[2])
erros_conj4_2 <- identificar_valores_fora_intervalo(conj4_base2, limites_conj4[1], limites_conj4[2])
erros_conj5_2 <- identificar_valores_fora_intervalo(conj5_base2, limites_conj5[1], limites_conj5[2])


# Exibir os erros para cada conjunto

#Conjunto 2

if (nrow(erros_conj2_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2_2)) {
    linha <- erros_conj2_2[i, 1]
    coluna <- erros_conj2_2[i, 2]
    valor <- conj2_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


#Conjunto 3
if (nrow(erros_conj3_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3_2)) {
    linha <- erros_conj3_2[i, 1]
    coluna <- erros_conj3_2[i, 2]
    valor <- conj3_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto 4
if (nrow(erros_conj4_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4_2)) {
    linha <- erros_conj4_2[i, 1]
    coluna <- erros_conj4_2[i, 2]
    valor <- conj4_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto5
if (nrow(erros_conj5_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5_2)) {
    linha <- erros_conj5_2[i, 1]
    coluna <- erros_conj5_2[i, 2]
    valor <- conj5_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

###################### BOX PLOTS 2 #####################################

#acessar um dos dataframes resultantes 
conj1_base2 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base2<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base2<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base2 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base2<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base2<-dataframes_conjuntos$conjunto_6 %>% 
  gather()

#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base2$key, levels = variaveis)

ggplot(data = conj1_base2, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 3") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 4") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 5") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base2$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.1, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base2$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base2$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

##ESTATISTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base2 <- dataframes_conjuntos$conjunto_1
conj2_base2<- dataframes_conjuntos$conjunto_2
conj3_base2<-dataframes_conjuntos$conjunto_3 
conj4_base2 <-dataframes_conjuntos$conjunto_4
conj5_base2<-dataframes_conjuntos$conjunto_5 
conj6_base2<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados.2 <- list(
  conj1_base2,
  conj2_base2,
  conj3_base2,
  conj4_base2,
  conj5_base2,
  conj6_base2
)

# Lista para armazenar os resumos estatísticos
resumos.2 <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados.2) {
  resumos.2[[length(resumos.2) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana
resumos.2

##################################################

# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código
# Iterar pelas colunas de df_2 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_2[, col][df_2[, col] < min_limit | 
                df_2[, col] > max_limit] <- NA
}

# Removendo NA's
df_2 <- na.omit(df_2)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_2_standard <- as.data.frame(scale(df_2))

#######REGRESSÃO E SCATTLEPLOT-BASE 2####################
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_2_reg <-df_2_standard[, c("y", paste0("V", 1:200))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg3 <- lm(y ~ ., data = df_2_reg)
summary(reg3)
stargazer(reg3, type = "text")
reg4=lm(y~., data=df_2_standard)
summary(reg4)
stargazer(reg4, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_2_standard$y,df_2_standard$V1, xlab = "V1", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V56
plot(df_2_standard$y,df_2_standard$V57, xlab = "V57", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V112
plot(df_2_standard$y,df_2_standard$V112,xlab = "V112", ylab = "Y")
abline(reg1)

#Regressão com todas as variáveis(Como não possível testar a significância, foi considerado apenas a variável V1)
plot(df_2_standard$y,df_2_standard$V1,xlab = "V1", ylab = "Y")
abline(reg4)


#--------------- TAREFA 2 ------------------------------------------------------

# Já instalamos o pacote caTools

#----------Dividindo os dados em Treinamento (30%) e Teste (70%)----------------

#A separação dos dados em conjuntos de treinamento e teste é uma prática fundamental 
#no aprendizado de máquina. O conjunto de treinamento é usado para ajustar o modelo, 
#permitindo que ele aprenda com os dados disponíveis. O conjunto 
#de teste é utilizado para avaliar o desempenho do modelo, proporcionando uma medida
#objetiva de quão bem o modelo generaliza para novos dados não utilizados durante o treinamento. 
#Essa divisão é crucial para evitar superajuste, comparar diferentes modelos e tomar decisões informadas 
#sobre a capacidade de um modelo de prever com precisão em situações reais.

#Vamos usar a função sample.split() para dividir nosso dados em 30% e 70%


### BASE DE DADOS 1
set.seed(123) # garantindo a reprodutibilidade
split <- sample.split(df_1$y, SplitRatio = 0.7)
training <- subset(df_1, split == TRUE)
testing <- subset(df_1, split == FALSE)


### BASE DE DADOS 2
set.seed(123) # garantindo a reprodutibilidade
split2 <- sample.split(df_2$y, SplitRatio = 0.7)
training2 <- subset(df_2, split == TRUE)
testing2 <- subset(df_2, split == FALSE)




df_1 <- read_csv("data_1_grupo_8.csv")
df_2 <- read_csv("data_2_grupo_8.csv")

### 1) identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base1 <- function(colunas) {
  conjunto_atual <- df_1[, colunas]
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}

# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base1(conjunto)
}

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos <- list()

# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_1[, conjunto]
  dataframes_conjuntos[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1 <- dataframes_conjuntos$conjunto_2
conj3_base1 <- dataframes_conjuntos$conjunto_3
conj4_base1 <- dataframes_conjuntos$conjunto_4
conj5_base1 <- dataframes_conjuntos$conjunto_5
conj6_base1 <- dataframes_conjuntos$conjunto_6

# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("No NAs found in", nome_dataframe, "\n")
  } else {
    cat("NAs found in", nome_dataframe, "at the following rows and columns:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Row:", linha, "Column:", coluna, "\n")
    }
  }
}


### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b1 <- which(df_1[, 31:60] < 0 | df_1[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b1)) {
    linha <- erros_c2_b1[i, 1]
    coluna <- 30 + erros_c2_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b1 <- which(df_1[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b1)) {
    linha <- erros_c3_b1[i, 1]
    coluna <- 60 + erros_c3_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b1 <- which(df_1[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b1)) {
    linha <- erros_c4_b1[i, 1]
    coluna <- 90 + erros_c4_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_1[, 121:150] < 0 | df_1[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


## Dentro de cada novo Dataframe:

# Função para identificar valores fora do intervalo
identificar_valores_fora_intervalo <- function(dataframe, limite_inferior, limite_superior) {
  erros <- which(dataframe < limite_inferior | dataframe > limite_superior, arr.ind = TRUE)
  return(erros)
}

# Definir limites de intervalo para cada conjunto
limites_conj2 <- c(0, 1)
limites_conj3 <- c(0, Inf)
limites_conj4 <- c(-Inf, 0)
limites_conj5 <- c(0, 300)

# Identificar valores fora do intervalo para cada conjunto
erros_conj2 <- identificar_valores_fora_intervalo(conj2_base1, limites_conj2[1], limites_conj2[2])
erros_conj3 <- identificar_valores_fora_intervalo(conj3_base1, limites_conj3[1], limites_conj3[2])
erros_conj4 <- identificar_valores_fora_intervalo(conj4_base1, limites_conj4[1], limites_conj4[2])
erros_conj5 <- identificar_valores_fora_intervalo(conj5_base1, limites_conj5[1], limites_conj5[2])

# Exibir os erros para cada conjunto
if (nrow(erros_conj2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2)) {
    linha <- erros_conj2[i, 1]
    coluna <- erros_conj2[i, 2]
    valor <- conj2_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj3) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3)) {
    linha <- erros_conj3[i, 1]
    coluna <- erros_conj3[i, 2]
    valor <- conj3_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj4) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4)) {
    linha <- erros_conj4[i, 1]
    coluna <- erros_conj4[i, 2]
    valor <- conj4_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj5) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 5.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 5 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5)) {
    linha <- erros_conj5[i, 1]
    coluna <- erros_conj5[i, 2]
    valor <- conj5_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}



###################### BOX PLOTS #####################################

#acessar um dos dataframes resultantes 
conj1_base1 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base1<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base1<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base1 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base1<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base1<-dataframes_conjuntos$conjunto_6 %>% 
  gather()


#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base1$key, levels = variaveis)

ggplot(data = conj1_base1, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base1$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base1[1:8500,], aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base1$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base1$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")


##ESTATÍSTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1<- dataframes_conjuntos$conjunto_2
conj3_base1<-dataframes_conjuntos$conjunto_3 
conj4_base1 <-dataframes_conjuntos$conjunto_4
conj5_base1<-dataframes_conjuntos$conjunto_5 
conj6_base1<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados <- list(
  conj1_base1,
  conj2_base1,
  conj3_base1,
  conj4_base1,
  conj5_base1,
  conj6_base1
)

# Lista para armazenar os resumos estatísticos
resumos <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados) {
  resumos[[length(resumos) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana

resumos


# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código

# Definindo um dataframe com os limites de cada variável

limits <- as.data.frame(matrix(NA, nrow = 2, ncol = 250))

for(i in 1:30) {
  limits[[i]] <- c(-Inf, Inf)
  limits[[i+30]] <- c(0, 1)
  limits[[i+60]] <- c(0, Inf)
  limits[[i+90]] <- c(-Inf, 0)
  limits[[i+120]] <- c(0, 300)
}

for(i in 151:250) {
  limits[[i]] <- c(-Inf, Inf)
}

# Iterar pelas colunas de df_1 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_1[, col][df_1[, col] < min_limit | 
                df_1[, col] > max_limit] <- NA
}

# Removendo NA's
df_1 <- na.omit(df_1)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_1_standard <- as.data.frame(scale(df_1))

###########REGRESSÃO E SCATTLEPLOT-BASE 1###############
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_1_reg <-df_1_standard[, c("y", paste0("V", 1:205))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg1 <- lm(y ~ ., data = df_1_reg)
summary(reg1)
stargazer(reg1, type = "text")
reg2=lm(y~., data=df_1_standard)
summary(reg2)
stargazer(reg1, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_1_standard$y,df_1_standard$V1,xlab = "V1", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V56
plot(df_1_standard$y,df_1_standard$V56,xlab = "V56", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V112
plot(df_1_standard$y,df_1_standard$V112, xlab = "V112", ylab = "Y")
abline(reg1)

#Gráfco da regressão com todas as variáveis
plot(df_1_standard$y,df_1_standard$V1, xlab = "V1", ylab = "Y")
abline(reg2)

(AQUI COLOCAR O RESTO DO CODIGO PRA BASE 1)











#### - BASE DE DADOS 2----------------------------------------------------------

#### - BASE DE DADOS 2----------------------------------------------------------

### 1) Identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base2 <- function(colunas) {
  # Selecionar apenas as colunas do conjunto atual
  conjunto_atual <- df_2[, colunas]
  
  # Identificar valores NA no conjunto atual
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  # Exibir resultados para o conjunto atual
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}


# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base2(conjunto)
}

# Criando data frames para cada conjunto da base 2

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos2 <- list()


# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_2[, conjunto]
  dataframes_conjuntos2[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base2 <- dataframes_conjuntos2$conjunto_1
conj2_base2 <- dataframes_conjuntos2$conjunto_2
conj3_base2 <- dataframes_conjuntos2$conjunto_3
conj4_base2 <- dataframes_conjuntos2$conjunto_4
conj5_base2 <- dataframes_conjuntos2$conjunto_5
conj6_base2 <- dataframes_conjuntos2$conjunto_6


# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na2 <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("Nenhum NA encontrado em", nome_dataframe, "\n")
  } else {
    cat("NAs encontrados em", nome_dataframe, "nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", coluna, "\n")
    }
  }
}
### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b2 <- which(df_2[, 31:60] < 0 | df_2[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60 no Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b2)) {
    linha <- erros_c2_b2[i, 1]
    coluna <- 30 + erros_c2_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}




# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b2 <- which(df_2[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b2)) {
    linha <- erros_c3_b2[i, 1]
    coluna <- 60 + erros_c3_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b2 <- which(df_2[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b2)) {
    linha <- erros_c4_b2[i, 1]
    coluna <- 90 + erros_c4_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_2[, 121:150] < 0 | df_2[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}

## Dentro de cada novo Dataframe:

# Identificar valores fora do intervalo para cada conjunto
erros_conj2_2 <- identificar_valores_fora_intervalo(conj2_base2, limites_conj2[1], limites_conj2[2])
erros_conj3_2<- identificar_valores_fora_intervalo(conj3_base2, limites_conj3[1], limites_conj3[2])
erros_conj4_2 <- identificar_valores_fora_intervalo(conj4_base2, limites_conj4[1], limites_conj4[2])
erros_conj5_2 <- identificar_valores_fora_intervalo(conj5_base2, limites_conj5[1], limites_conj5[2])


# Exibir os erros para cada conjunto

#Conjunto 1

if (nrow(erros_conj2_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2_2)) {
    linha <- erros_conj2_2[i, 1]
    coluna <- erros_conj2_2[i, 2]
    valor <- conj2_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


#Conjunto 3
if (nrow(erros_conj3_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3_2)) {
    linha <- erros_conj3_2[i, 1]
    coluna <- erros_conj3_2[i, 2]
    valor <- conj3_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto 4
if (nrow(erros_conj4_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4_2)) {
    linha <- erros_conj4_2[i, 1]
    coluna <- erros_conj4_2[i, 2]
    valor <- conj4_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto54
if (nrow(erros_conj5_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5_2)) {
    linha <- erros_conj5_2[i, 1]
    coluna <- erros_conj5_2[i, 2]
    valor <- conj5_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

###################### BOX PLOTS 2 #####################################

#acessar um dos dataframes resultantes 
conj1_base2 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base2<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base2<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base2 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base2<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base2<-dataframes_conjuntos$conjunto_6 %>% 
  gather()

#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base2$key, levels = variaveis)

ggplot(data = conj1_base2, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 3") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 4") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 5") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base2$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.1, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base2$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base2$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

##ESTATISTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base2 <- dataframes_conjuntos$conjunto_1
conj2_base2<- dataframes_conjuntos$conjunto_2
conj3_base2<-dataframes_conjuntos$conjunto_3 
conj4_base2 <-dataframes_conjuntos$conjunto_4
conj5_base2<-dataframes_conjuntos$conjunto_5 
conj6_base2<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados.2 <- list(
  conj1_base2,
  conj2_base2,
  conj3_base2,
  conj4_base2,
  conj5_base2,
  conj6_base2
)

# Lista para armazenar os resumos estatísticos
resumos.2 <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados.2) {
  resumos.2[[length(resumos.2) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana
resumos.2

##################################################

# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código
# Iterar pelas colunas de df_2 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_2[, col][df_2[, col] < min_limit | 
                df_2[, col] > max_limit] <- NA
}

# Removendo NA's
df_2 <- na.omit(df_2)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_2_standard <- as.data.frame(scale(df_2))

#######REGRESSÃO E SCATTLEPLOT-BASE 2####################
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_2_reg <-df_2_standard[, c("y", paste0("V", 1:200))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg3 <- lm(y ~ ., data = df_2_reg)
summary(reg3)
stargazer(reg3, type = "text")
reg4=lm(y~., data=df_2_standard)
summary(reg4)
stargazer(reg4, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_2_standard$y,df_2_standard$V1, xlab = "V1", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V56
plot(df_2_standard$y,df_2_standard$V57, xlab = "V57", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V112
plot(df_2_standard$y,df_2_standard$V112,xlab = "V112", ylab = "Y")
abline(reg1)

#Regressão com todas as variáveis(Como não possível testar a significância, foi considerado apenas a variável V1)
plot(df_2_standard$y,df_2_standard$V1,xlab = "V1", ylab = "Y")
abline(reg4)




TAREFA 2



#################

#TAREFA 2 - Modelos Lineares Esparsos ou Densos
# GRUPO 8
# Outubro, 2023

#################

# 0. Set-up
# =================================================================

rm(list=ls())

# pacotes

packages<-c("dplyr", "readr", "stargazer",
            "ggplot2", "knitr", "tidyr", "tinytex",
            "rmarkdown","caTools")
to_install<-packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install)>0) install.packages(to_install)

lapply(packages,require,character.only=TRUE)


# DIRETÓRIO
# ------------------------------------


setwd("C:/Users/nfrig/OneDrive/Desktop/trabalhoDS")

#--------------- CODIGO REFERENTE à TAREFA 1 (Pré-Processamento)-----------------


# Importando as bases de dados

df_1 <- read_csv("data_1_grupo_8.csv")
df_2 <- read_csv("data_2_grupo_8.csv")

### 1) identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base1 <- function(colunas) {
  conjunto_atual <- df_1[, colunas]
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}

# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base1(conjunto)
}

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos <- list()

# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_1[, conjunto]
  dataframes_conjuntos[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1 <- dataframes_conjuntos$conjunto_2
conj3_base1 <- dataframes_conjuntos$conjunto_3
conj4_base1 <- dataframes_conjuntos$conjunto_4
conj5_base1 <- dataframes_conjuntos$conjunto_5
conj6_base1 <- dataframes_conjuntos$conjunto_6

# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("No NAs found in", nome_dataframe, "\n")
  } else {
    cat("NAs found in", nome_dataframe, "at the following rows and columns:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Row:", linha, "Column:", coluna, "\n")
    }
  }
}


### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b1 <- which(df_1[, 31:60] < 0 | df_1[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b1)) {
    linha <- erros_c2_b1[i, 1]
    coluna <- 30 + erros_c2_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b1 <- which(df_1[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b1)) {
    linha <- erros_c3_b1[i, 1]
    coluna <- 60 + erros_c3_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b1 <- which(df_1[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b1)) {
    linha <- erros_c4_b1[i, 1]
    coluna <- 90 + erros_c4_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_1[, 121:150] < 0 | df_1[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_1[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


## Dentro de cada novo Dataframe:

# Função para identificar valores fora do intervalo
identificar_valores_fora_intervalo <- function(dataframe, limite_inferior, limite_superior) {
  erros <- which(dataframe < limite_inferior | dataframe > limite_superior, arr.ind = TRUE)
  return(erros)
}

# Definir limites de intervalo para cada conjunto
limites_conj2 <- c(0, 1)
limites_conj3 <- c(0, Inf)
limites_conj4 <- c(-Inf, 0)
limites_conj5 <- c(0, 300)

# Identificar valores fora do intervalo para cada conjunto
erros_conj2 <- identificar_valores_fora_intervalo(conj2_base1, limites_conj2[1], limites_conj2[2])
erros_conj3 <- identificar_valores_fora_intervalo(conj3_base1, limites_conj3[1], limites_conj3[2])
erros_conj4 <- identificar_valores_fora_intervalo(conj4_base1, limites_conj4[1], limites_conj4[2])
erros_conj5 <- identificar_valores_fora_intervalo(conj5_base1, limites_conj5[1], limites_conj5[2])

# Exibir os erros para cada conjunto
if (nrow(erros_conj2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2)) {
    linha <- erros_conj2[i, 1]
    coluna <- erros_conj2[i, 2]
    valor <- conj2_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj3) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3)) {
    linha <- erros_conj3[i, 1]
    coluna <- erros_conj3[i, 2]
    valor <- conj3_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj4) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4)) {
    linha <- erros_conj4[i, 1]
    coluna <- erros_conj4[i, 2]
    valor <- conj4_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

if (nrow(erros_conj5) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 5.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 5 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5)) {
    linha <- erros_conj5[i, 1]
    coluna <- erros_conj5[i, 2]
    valor <- conj5_base1[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}



###################### BOX PLOTS #####################################

#acessar um dos dataframes resultantes 
conj1_base1 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base1<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base1<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base1 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base1<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base1<-dataframes_conjuntos$conjunto_6 %>% 
  gather()


#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base1$key, levels = variaveis)

ggplot(data = conj1_base1, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base1, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base1$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base1[1:8500,], aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base1$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base1$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base1, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 1") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")


##ESTATÍSTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base1 <- dataframes_conjuntos$conjunto_1
conj2_base1<- dataframes_conjuntos$conjunto_2
conj3_base1<-dataframes_conjuntos$conjunto_3 
conj4_base1 <-dataframes_conjuntos$conjunto_4
conj5_base1<-dataframes_conjuntos$conjunto_5 
conj6_base1<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados <- list(
  conj1_base1,
  conj2_base1,
  conj3_base1,
  conj4_base1,
  conj5_base1,
  conj6_base1
)

# Lista para armazenar os resumos estatísticos
resumos <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados) {
  resumos[[length(resumos) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana

resumos


# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código

# Definindo um dataframe com os limites de cada variável

limits <- as.data.frame(matrix(NA, nrow = 2, ncol = 250))

for(i in 1:30) {
  limits[[i]] <- c(-Inf, Inf)
  limits[[i+30]] <- c(0, 1)
  limits[[i+60]] <- c(0, Inf)
  limits[[i+90]] <- c(-Inf, 0)
  limits[[i+120]] <- c(0, 300)
}

for(i in 151:250) {
  limits[[i]] <- c(-Inf, Inf)
}

# Iterar pelas colunas de df_1 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_1[, col][df_1[, col] < min_limit | 
                df_1[, col] > max_limit] <- NA
}

# Removendo NA's
df_1 <- na.omit(df_1)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_1_standard <- as.data.frame(scale(df_1))

###########REGRESSÃO E SCATTLEPLOT-BASE 1###############
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_1_reg <-df_1_standard[, c("y", paste0("V", 1:205))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg1 <- lm(y ~ ., data = df_1_reg)
summary(reg1)
stargazer(reg1, type = "text")
reg2=lm(y~., data=df_1_standard)
summary(reg2)
stargazer(reg1, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_1_standard$y,df_1_standard$V1,xlab = "V1", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V56
plot(df_1_standard$y,df_1_standard$V56,xlab = "V56", ylab = "Y")
abline(reg1)
##Gráfico entre Y e V112
plot(df_1_standard$y,df_1_standard$V112, xlab = "V112", ylab = "Y")
abline(reg1)

#Gráfco da regressão com todas as variáveis
plot(df_1_standard$y,df_1_standard$V1, xlab = "V1", ylab = "Y")
abline(reg2)


#### - BASE DE DADOS 2----------------------------------------------------------

### 1) Identificando NAs

# Conjuntos de colunas
conjuntos_colunas <- list(c(1:30), c(31:60), c(61:90), c(91:120), c(121:150), c(151:250))

# Função para identificar e exibir valores NA em um conjunto de colunas
identificar_na_base2 <- function(colunas) {
  # Selecionar apenas as colunas do conjunto atual
  conjunto_atual <- df_2[, colunas]
  
  # Identificar valores NA no conjunto atual
  na_indices <- which(is.na(conjunto_atual), arr.ind = TRUE)
  
  # Exibir resultados para o conjunto atual
  cat("Conjunto de colunas:", colunas[1], "-", colunas[length(colunas)], "\n")
  if (length(na_indices) == 0) {
    cat("Nenhum valor NA encontrado.\n")
  } else {
    cat("Valores NA encontrados nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", colunas[coluna], "\n")
    }
  }
}


# Iterar através dos conjuntos de colunas e identificar NA
for (conjunto in conjuntos_colunas) {
  identificar_na_base2(conjunto)
}

# Criando data frames para cada conjunto da base 2

# Lista de dataframes para armazenar os conjuntos de colunas
dataframes_conjuntos2 <- list()


# Iterar através dos conjuntos de colunas e criar dataframes separados
for (i in 1:length(conjuntos_colunas)) {
  conjunto <- conjuntos_colunas[[i]]
  nome_dataframe <- paste("conjunto_", i, sep = "")
  dataframe_conjunto <- df_2[, conjunto]
  dataframes_conjuntos2[[nome_dataframe]] <- dataframe_conjunto
}

# Acessar um dos dataframes resultantes
conj1_base2 <- dataframes_conjuntos2$conjunto_1
conj2_base2 <- dataframes_conjuntos2$conjunto_2
conj3_base2 <- dataframes_conjuntos2$conjunto_3
conj4_base2 <- dataframes_conjuntos2$conjunto_4
conj5_base2 <- dataframes_conjuntos2$conjunto_5
conj6_base2 <- dataframes_conjuntos2$conjunto_6


# Função para identificar linhas e colunas com NA em um dataframe
identificar_linhas_colunas_na2 <- function(dataframe) {
  na_indices <- which(is.na(dataframe), arr.ind = TRUE)
  return(na_indices)
}

# Iterar através dos dataframes e identificar linhas e colunas com NA
for (nome_dataframe in names(dataframes_conjuntos)) {
  dataframe <- dataframes_conjuntos[[nome_dataframe]]
  na_indices <- identificar_linhas_colunas_na(dataframe)
  
  if (length(na_indices) == 0) {
    cat("Nenhum NA encontrado em", nome_dataframe, "\n")
  } else {
    cat("NAs encontrados em", nome_dataframe, "nas seguintes linhas e colunas:\n")
    for (i in 1:nrow(na_indices)) {
      linha <- na_indices[i, 1]
      coluna <- na_indices[i, 2]
      cat("Linha:", linha, "Coluna:", coluna, "\n")
    }
  }
}
### 2) Indentificando valores fora do intervalo

# conjunto 2: V31-60: 0 a 1

# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c2_b2 <- which(df_2[, 31:60] < 0 | df_2[, 31:60] > 1, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c2_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 31 a 60 no Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas no Conjunto 2:\n")
  for (i in 1:nrow(erros_c2_b2)) {
    linha <- erros_c2_b2[i, 1]
    coluna <- 30 + erros_c2_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}




# conjunto 3: V61-90: 0 a +infinito
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c3_b2 <- which(df_2[, 61:90] < 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c3_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 61 a 90.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c3_b2)) {
    linha <- erros_c3_b2[i, 1]
    coluna <- 60 + erros_c3_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 4 (91-120): -infinito a 0
# Identificar linhas e colunas onde os valores não se adequam à regra
erros_c4_b2 <- which(df_2[, 91:120] > 0, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c4_b2) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 91 a 120.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c4_b2)) {
    linha <- erros_c4_b2[i, 1]
    coluna <- 90 + erros_c4_b2[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}


#conjunto 5 (121-150): 0 a 300

erros_c5_b1 <- which(df_2[, 121:150] < 0 | df_2[, 121:150] > 300, arr.ind = TRUE)

# Exibir as linhas e colunas fora do intervalo
if (nrow(erros_c5_b1) == 0) {
  cat("Nenhum valor fora do intervalo encontrado nas colunas 121 a 150.\n")
} else {
  cat("Valores fora do intervalo encontrados nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_c5_b1)) {
    linha <- erros_c5_b1[i, 1]
    coluna <- 120 + erros_c5_b1[i, 2]  # Ajuste para obter o número da coluna original
    valor <- df_2[linha, coluna]
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", as.character(valor), "\n")
  }
}

## Dentro de cada novo Dataframe:

# Identificar valores fora do intervalo para cada conjunto
erros_conj2_2 <- identificar_valores_fora_intervalo(conj2_base2, limites_conj2[1], limites_conj2[2])
erros_conj3_2<- identificar_valores_fora_intervalo(conj3_base2, limites_conj3[1], limites_conj3[2])
erros_conj4_2 <- identificar_valores_fora_intervalo(conj4_base2, limites_conj4[1], limites_conj4[2])
erros_conj5_2 <- identificar_valores_fora_intervalo(conj5_base2, limites_conj5[1], limites_conj5[2])


# Exibir os erros para cada conjunto

#Conjunto 2

if (nrow(erros_conj2_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 2.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 2 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj2_2)) {
    linha <- erros_conj2_2[i, 1]
    coluna <- erros_conj2_2[i, 2]
    valor <- conj2_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


#Conjunto 3
if (nrow(erros_conj3_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 3.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 3 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj3_2)) {
    linha <- erros_conj3_2[i, 1]
    coluna <- erros_conj3_2[i, 2]
    valor <- conj3_base2[linha, coluna]
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto 4
if (nrow(erros_conj4_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj4_2)) {
    linha <- erros_conj4_2[i, 1]
    coluna <- erros_conj4_2[i, 2]
    valor <- conj4_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}


# Conjunto5
if (nrow(erros_conj5_2) == 0) {
  cat("Nenhum valor fora do intervalo para o Conjunto 4.\n")
} else {
  cat("Valores fora do intervalo para o Conjunto 4 nas seguintes linhas e colunas:\n")
  for (i in 1:nrow(erros_conj5_2)) {
    linha <- erros_conj5_2[i, 1]
    coluna <- erros_conj5_2[i, 2]
    valor <- conj5_base2[linha, coluna]  # Aqui deve ser conj4_base2, não erros_conj4_2
    valor_texto <- as.character(valor)  # Converter o valor para texto
    cat("Linha:", linha, "Coluna:", coluna, "Valor:", valor_texto, "\n")
  }
}

###################### BOX PLOTS 2 #####################################

#acessar um dos dataframes resultantes 
conj1_base2 <- dataframes_conjuntos$conjunto_1 %>% 
  gather()

conj2_base2<- dataframes_conjuntos$conjunto_2 %>% 
  gather()

conj3_base2<-dataframes_conjuntos$conjunto_3 %>% 
  gather()

conj4_base2 <-dataframes_conjuntos$conjunto_4 %>% 
  gather()

conj5_base2<-dataframes_conjuntos$conjunto_5 %>% 
  gather()

conj6_base2<-dataframes_conjuntos$conjunto_6 %>% 
  gather()

#BOXPLOTS::::::::::::

#Conjunto 1
##PRECISO DEIXAR EM ORDEM CRESCENTE (X)
# Crie um vetor com a ordem desejada das variáveis
variaveis <- paste0("V", 1:30)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator <- factor(conj1_base2$key, levels = variaveis)

ggplot(data = conj1_base2, aes(x = variaveis_fator, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 1 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 2
ggplot(data = conj2_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 2 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 3
ggplot(data = conj3_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 3 da Base 3") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 4
ggplot(data = conj4_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 4 da Base 4") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 5
ggplot(data = conj5_base2, aes(x = key, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot para Conjunto 5 da Base 5") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

#Conjunto 6:: OBS PRECISO TIRAR O NA (QUE É O BOXPLOT CRIADO PELOS OUTROS IMPUTS NA BASE)

#Como o conjunto 6 são 99 elementos 151:250
#Vamos separar em 3 gráficos com 33 valores em cada

# Crie um vetor com as variáveis de "V151" até "V184"
variaveis_conj6.1 <- paste0("V", 151:184)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.1 <- factor(conj6_base2$key, levels = variaveis_conj6.1)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.1, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 1) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V185" até "V217"
variaveis_conj6.2 <- paste0("V", 185:217)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.2 <- factor(conj6_base2$key, levels = variaveis_conj6.2)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.2, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 2) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

# Crie um vetor com as variáveis de "V218" até "V250"
variaveis_conj6.3 <- paste0("V", 218:250)

# Converta as variáveis em um fator com a ordem desejada
variaveis_fator.conj6.3 <- factor(conj6_base2$key, levels = variaveis_conj6.3)

ggplot(data = conj6_base2, aes(x = variaveis_fator.conj6.3, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot (Parte 3) para Conjunto 6 da Base 2") +
  theme_minimal() +
  xlab("Variáveis") +
  ylab("Valores")

##ESTATISTICAS DESCRITIVAS  

#Retornando o data frame antigo:
conj1_base2 <- dataframes_conjuntos$conjunto_1
conj2_base2<- dataframes_conjuntos$conjunto_2
conj3_base2<-dataframes_conjuntos$conjunto_3 
conj4_base2 <-dataframes_conjuntos$conjunto_4
conj5_base2<-dataframes_conjuntos$conjunto_5 
conj6_base2<-dataframes_conjuntos$conjunto_6

# Lista de bases de dados
bases_de_dados.2 <- list(
  conj1_base2,
  conj2_base2,
  conj3_base2,
  conj4_base2,
  conj5_base2,
  conj6_base2
)

# Lista para armazenar os resumos estatísticos
resumos.2 <- list()

# Calcular o resumo estatístico para cada base e armazenar na lista
for (base in bases_de_dados.2) {
  resumos.2[[length(resumos.2) + 1]] <- summary(base)
}

#Valor dos Quartis, media e mediana
resumos.2

##################################################

# Primeiro, colocamos todos os outliers como NA's. Eles serão 
# removidos mais à frente no código
# Iterar pelas colunas de df_2 e limites para trocar outliers por NAs

for (col in 1:250) {
  # Obter os limites mínimo e máximo da coluna atual
  min_limit <- limits[1, col]
  max_limit <- limits[2, col]
  
  # Substituir os valores fora dos limites por NA
  df_2[, col][df_2[, col] < min_limit | 
                df_2[, col] > max_limit] <- NA
}

# Removendo NA's
df_2 <- na.omit(df_2)

# Padronizando as variáveis (subtraímos cada coluna pela média, 
# e dividimos pelo seu respectivo desvio padrão). Isso é feito
# automaticamente pela função scale

df_2_standard <- as.data.frame(scale(df_2))

#######REGRESSÃO E SCATTLEPLOT-BASE 2####################
#Alterando a ordem das colunas, deixando y como a primeira, para facilitar o código da regressão
df_2_reg <-df_2_standard[, c("y", paste0("V", 1:200))] 
# Realizando duas regressões em que uma utiliza todas as variáveis da base e outra considera 
# o numero máximo de váriáveis até atingir o overfitting do modelo. 
reg3 <- lm(y ~ ., data = df_2_reg)
summary(reg3)
stargazer(reg3, type = "text")
reg4=lm(y~., data=df_2_standard)
summary(reg4)
stargazer(reg4, type = "text")

# Realizando os Scatterplots comparando o Y real com o predito no modelo, 
# utilizando variáveis X com diferentes limites(Escolhidos pela nível de maior significância)
#Regressão até a variável V205
#Gráfico entre Y e V1
plot(df_2_standard$y,df_2_standard$V1, xlab = "V1", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V56
plot(df_2_standard$y,df_2_standard$V57, xlab = "V57", ylab = "Y")
abline(reg3)
##Gráfico entre Y e V112
plot(df_2_standard$y,df_2_standard$V112,xlab = "V112", ylab = "Y")
abline(reg1)

#Regressão com todas as variáveis(Como não possível testar a significância, foi considerado apenas a variável V1)
plot(df_2_standard$y,df_2_standard$V1,xlab = "V1", ylab = "Y")
abline(reg4)


#--------------- TAREFA 2 ------------------------------------------------------

# Já instalamos o pacote caTools

#----------Dividindo os dados em Treinamento (30%) e Teste (70%)----------------

#A separação dos dados em conjuntos de treinamento e teste é uma prática fundamental 
#no aprendizado de máquina. O conjunto de treinamento é usado para ajustar o modelo, 
#permitindo que ele aprenda com os dados disponíveis. O conjunto 
#de teste é utilizado para avaliar o desempenho do modelo, proporcionando uma medida
#objetiva de quão bem o modelo generaliza para novos dados não utilizados durante o treinamento. 
#Essa divisão é crucial para evitar superajuste, comparar diferentes modelos e tomar decisões informadas 
#sobre a capacidade de um modelo de prever com precisão em situações reais.

#Vamos usar a função sample.split() para dividir nosso dados em 30% e 70%


### BASE DE DADOS 1
set.seed(123) # garantindo a reprodutibilidade
split <- sample.split(df_1$y, SplitRatio = 0.7)
training <- subset(df_1, split == TRUE)
testing <- subset(df_1, split == FALSE)


### BASE DE DADOS 2
set.seed(123) # garantindo a reprodutibilidade
split2 <- sample.split(df_2$y, SplitRatio = 0.7)
training2 <- subset(df_2, split == TRUE)
testing2 <- subset(df_2, split == FALSE)


