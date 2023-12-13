@Title: Analise de Dados da Simulacao
@Author: Ramon M. Teixeira
@Date: 2023-12-04
@Version: 1.0.0

###############################################
### Perguntas a Serem Respondidas ###

# Com base nos resultados obtidos atraves da simulacao do modelo
# de "Life Game" (adaptado para a dinamica da relacao predador, presa e
# substrato) e do estabelecimento de parametros previamente determinados,
# devemos buscar a resposta para as seguintes perguntas:

# 1. O aumento da quantidade de substrato inicial impacta positivamente no 
# tempo de extincao dos predadores?

# 2. Existe diferenca significativa no tempo de extincao no caso do ambiente 
# comecar em maioria com predadores (3:1) comparado com a maioria 
# ser de presas (3:1)?

##############################################
### Analise Exploratoria ###

planilha <- read.csv("planilha.csv", header = TRUE, sep = ",")

nomeDasColunas <- colnames(planilha)
nomeDasColunas

# Coletando dados para a pergunta (1)

# Temos experimentos com 300, 400, 500, 600 e 700 de substrato
# Para cada substrato temos testes com maioria de predadores e maioria de presas
# Para cada um desses experimentos, temos 3 simulacoes

# filtrar planilha com 300 de substrato e 50 de predadores
substrato300E50DePredadoresInicialExp1 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 1,]
substrato300E50DePredadoresInicialExp2 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 2,]
substrato300E50DePredadoresInicialExp3 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 3,]

# filtrar planilha com 400 de substrato e 50 de predadores
substrato400E50DePredadoresInicialExp4 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 4,]
substrato400E50DePredadoresInicialExp5 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 5,]
substrato400E50DePredadoresInicialExp6 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 6,]

# filtrar planilha com 500 de substrato e 50 de predadores
substrato500E50DePredadoresInicialExp7 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 7,]
substrato500E50DePredadoresInicialExp8 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 8,]
substrato500E50DePredadoresInicialExp9 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 9,]

# filtrar planilha com 600 de substrato e 50 de predadores
substrato600E50DePredadoresInicialExp10 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 10,]
substrato600E50DePredadoresInicialExp11 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 11,]
substrato600E50DePredadoresInicialExp12 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 12,]

# filtrar planilha com 700 de substrato e 50 de predadores
substrato700E50DePredadoresInicialExp13 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 13,]
substrato700E50DePredadoresInicialExp14 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 14,]
substrato700E50DePredadoresInicialExp15 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDoExperimento == 15,]


# Coletando dados para a pergunta (2)

# filtrar planilha com 300 de substrato e 150 de predadores
substrato300E150DePredadoresInicialExp16 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 16,]
substrato300E150DePredadoresInicialExp17 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 17,]
substrato300E150DePredadoresInicialExp18 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 18,]

# filtrar planilha com 400 de substrato e 150 de predadores
substrato400E150DePredadoresInicialExp19 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 19,]
substrato400E150DePredadoresInicialExp20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 20,]
substrato400E150DePredadoresInicialExp21 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 21,]

# filtrar planilha com 500 de substrato e 150 de predadores
substrato500E150DePredadoresInicialExp22 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 22,]
substrato500E150DePredadoresInicialExp23 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 23,]
substrato500E150DePredadoresInicialExp24 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 24,]

# filtrar planilha com 600 de substrato e 150 de predadores
substrato600E150DePredadoresInicialExp25 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 25,]
substrato600E150DePredadoresInicialExp26 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 26,]
substrato600E150DePredadoresInicialExp27 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 27,]

# filtrar planilha com 700 de substrato e 150 de predadores
substrato700E150DePredadoresInicialExp28 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 28,]
substrato700E150DePredadoresInicialExp29 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 29,]
substrato700E150DePredadoresInicialExp30 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDoExperimento == 30,]

plotarGrafico <- function(experimento1, experimento2, experimento3, nomeDoArquivo) {
    # Pego qualquer um dos tres experimentos para pegar o numero de geracoes
    x <- experimento1$NumeroDaGeracao

    y1 <- experimento1$NumeroDePredadoresVivos
    y2 <- experimento2$NumeroDePredadoresVivos
    y3 <- experimento3$NumeroDePredadoresVivos

    media_y <- numeric(length(y1))  # Cria um vetor vazio para armazenar as médias

    for (i in seq_along(y1)) {
        media_y[i] <- (y1[i] + y2[i] + y3[i]) / 3
    }

    # Salvar os dados em um arquivo png
    # Plot dos pontos

    png(nomeDoArquivo)
    plot(x, media_y, type = "o", col = "#f55435", xlab = "Geração", ylab = "Número de Predadores Vivos", las = 1, lwd=5)
    # Adiciona linhas conectando os pontos
    lines(x, media_y, col = "#f55435", lwd=5)

    dev.off()

}

# Plotando os graficos para cada experimento
plotarGrafico(substrato300E50DePredadoresInicialExp1, substrato300E50DePredadoresInicialExp2, substrato300E50DePredadoresInicialExp3, "substrato300Exp1Exp2Exp3.png")
plotarGrafico(substrato400E50DePredadoresInicialExp4, substrato400E50DePredadoresInicialExp5, substrato400E50DePredadoresInicialExp6, "substrato400Exp4Exp5Exp6.png")
plotarGrafico(substrato500E50DePredadoresInicialExp7, substrato500E50DePredadoresInicialExp8, substrato500E50DePredadoresInicialExp9, "substrato500Exp7Exp8Exp9.png")
plotarGrafico(substrato600E50DePredadoresInicialExp10, substrato600E50DePredadoresInicialExp11, substrato600E50DePredadoresInicialExp12, "substrato600Exp10Exp11Exp12.png")
plotarGrafico(substrato700E50DePredadoresInicialExp13, substrato700E50DePredadoresInicialExp14, substrato700E50DePredadoresInicialExp15, "substrato700Exp13Exp14Exp15.png")

plotarGrafico(substrato300E150DePredadoresInicialExp16, substrato300E150DePredadoresInicialExp17, substrato300E150DePredadoresInicialExp18, "substrato300Exp16Exp17Exp18.png")
plotarGrafico(substrato400E150DePredadoresInicialExp19, substrato400E150DePredadoresInicialExp20, substrato400E150DePredadoresInicialExp21, "substrato400Exp19Exp20Exp21.png")
plotarGrafico(substrato500E150DePredadoresInicialExp22, substrato500E150DePredadoresInicialExp23, substrato500E150DePredadoresInicialExp24, "substrato500Exp22Exp23Exp24.png")
plotarGrafico(substrato600E150DePredadoresInicialExp25, substrato600E150DePredadoresInicialExp26, substrato600E150DePredadoresInicialExp27, "substrato600Exp25Exp26Exp27.png")
plotarGrafico(substrato700E150DePredadoresInicialExp28, substrato700E150DePredadoresInicialExp29, substrato700E150DePredadoresInicialExp30, "substrato700Exp28Exp29Exp30.png")


# Pergunta (1):
# Existe diferenca significativa entre o aumento de substrato e o tempo de extincao dos predadores?
# Para responder essa pergunta, vamos utilizar o teste de hipotese ANOVA 

# Vamos juntar todos os dados em um unico dataframe
# Para isso, vamos criar uma coluna para indicar o numero de substrato inicial e 
# outra coluna para indicar o numero de predadores vivos

# Cada substrato deve ser categorico
substratoInicial50Predadores <- c(substrato300E50DePredadoresInicialExp1$NumeroDeSubstratosInicial, substrato300E50DePredadoresInicialExp2$NumeroDeSubstratosInicial, substrato300E50DePredadoresInicialExp3$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp4$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp5$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp6$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp7$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp8$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp9$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp10$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp11$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp12$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp13$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp14$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp15$NumeroDeSubstratosInicial)
substratoInicial50Predadores <- as.factor(substratoInicial)

predadoresVivos <- c(substrato300E50DePredadoresInicialExp1$NumeroDePredadoresVivos, substrato300E50DePredadoresInicialExp2$NumeroDePredadoresVivos, substrato300E50DePredadoresInicialExp3$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp4$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp5$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp6$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp7$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp8$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp9$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp10$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp11$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp12$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp13$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp14$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp15$NumeroDePredadoresVivos)

png("prop1_3ComZero.png")
boxplot(predadoresVivos ~ substratoInicial, data = planilha, main = "Predadores Vivos x Substrato Inicial", xlab = "Substrato Inicial", ylab = "Predadores Vivos", col = "#f55435")

planilhaPergunta1 <- data.frame(substratoInicial50Predadores, predadoresVivos)
dev.off()
library(car)

# Teste de Homogeneidade das Variancias [OK]
leveneTest(planilhaPergunta1$predadoresVivos ~ planilhaPergunta1$substratoInicial) # p = 0.923

# Teste de Normalidade
resultadoANOVA <- aov(predadoresVivos ~ substratoInicial50Predadores, data = planilhaPergunta1)

resultado2ANOVA <- lm(predadoresVivos ~ substratoInicial50Predadores, data = planilhaPergunta1)

summary(resultadoANOVA)
summary(resultado2ANOVA)

testeNaoParametrico <- kruskal.test(predadoresVivos ~ substratoInicial50Predadores, data = planilhaPergunta1)
testeNaoParametrico

# Repetindo para 150 predadores
substratoInicial150Predadores <- c(substrato300E150DePredadoresInicialExp16$NumeroDeSubstratosInicial, substrato300E150DePredadoresInicialExp17$NumeroDeSubstratosInicial, substrato300E150DePredadoresInicialExp18$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp19$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp20$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp21$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp22$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp23$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp24$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp25$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp26$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp27$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp28$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp29$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp30$NumeroDeSubstratosInicial)
substratoInicial150Predadores <- as.factor(substratoInicial150Predadores)

predadoresVivos150 <- c(substrato300E150DePredadoresInicialExp16$NumeroDePredadoresVivos, substrato300E150DePredadoresInicialExp17$NumeroDePredadoresVivos, substrato300E150DePredadoresInicialExp18$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp19$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp20$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp21$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp22$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp23$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp24$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp25$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp26$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp27$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp28$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp29$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp30$NumeroDePredadoresVivos)

png("prop3_1ComZero.png")
boxplot(predadoresVivos150 ~ substratoInicial150Predadores, data = planilha, main = "Predadores Vivos x Substrato Inicial", xlab = "Substrato Inicial", ylab = "Predadores Vivos", col = "#f55435")

planilhaPergunta1150 <- data.frame(substratoInicial150Predadores, predadoresVivos150)
dev.off()
library(car)

# Teste de Homogeneidade das Variancias [OK]
leveneTest(planilhaPergunta1150$predadoresVivos150 ~ planilhaPergunta1150$substratoInicial150Predadores) # p = 0.923

# Teste de Normalidade
resultadoANOVA150 <- aov(predadoresVivos150 ~ substratoInicial150Predadores, data = planilhaPergunta1150)

resultado2ANOVA150 <- lm(predadoresVivos150 ~ substratoInicial150Predadores, data = planilhaPergunta1150)

summary(resultadoANOVA150)
summary(resultado2ANOVA150)

testeNaoParametrico150 <- kruskal.test(predadoresVivos150 ~ substratoInicial150Predadores, data = planilhaPergunta1150)
testeNaoParametrico150



####### Limpando a Base de Dados - Considerando ate a geracao 20 ########

# Coletando dados para a pergunta (1)
substrato300E50DePredadoresInicialExp1AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 1,]
substrato300E50DePredadoresInicialExp2AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 2,]
substrato300E50DePredadoresInicialExp3AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 3,]

# filtrar planilha com 400 de substrato e 50 de predadores
substrato400E50DePredadoresInicialExp4AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 4,]
substrato400E50DePredadoresInicialExp5AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 5,]
substrato400E50DePredadoresInicialExp6AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 6,]

# filtrar planilha com 500 de substrato e 50 de predadores
substrato500E50DePredadoresInicialExp7AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 7,]
substrato500E50DePredadoresInicialExp8AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 8,]
substrato500E50DePredadoresInicialExp9AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 9,]

# filtrar planilha com 600 de substrato e 50 de predadores
substrato600E50DePredadoresInicialExp10AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 10,]
substrato600E50DePredadoresInicialExp11AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 11,]
substrato600E50DePredadoresInicialExp12AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 12,]

# filtrar planilha com 700 de substrato e 50 de predadores
substrato700E50DePredadoresInicialExp13AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 13,]
substrato700E50DePredadoresInicialExp14AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 14,]
substrato700E50DePredadoresInicialExp15AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 50 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 15,]


# Coletando dados para a pergunta (2)

# filtrar planilha com 300 de substrato e 150 de predadores
substrato300E150DePredadoresInicialExp16AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 16,]
substrato300E150DePredadoresInicialExp17AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 17,]
substrato300E150DePredadoresInicialExp18AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 300 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 18,]

# filtrar planilha com 400 de substrato e 150 de predadores
substrato400E150DePredadoresInicialExp19AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 19,]
substrato400E150DePredadoresInicialExp20AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 20,]
substrato400E150DePredadoresInicialExp21AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 400 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 21,]

# filtrar planilha com 500 de substrato e 150 de predadores
substrato500E150DePredadoresInicialExp22AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 22,]
substrato500E150DePredadoresInicialExp23AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 23,]
substrato500E150DePredadoresInicialExp24AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 500 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 24,]

# filtrar planilha com 600 de substrato e 150 de predadores
substrato600E150DePredadoresInicialExp25AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 25,]
substrato600E150DePredadoresInicialExp26AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 26,]
substrato600E150DePredadoresInicialExp27AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 600 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 27,]

# filtrar planilha com 700 de substrato e 150 de predadores
substrato700E150DePredadoresInicialExp28AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 28,]
substrato700E150DePredadoresInicialExp29AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 29,]
substrato700E150DePredadoresInicialExp30AteGeracao20 <- planilha[planilha$NumeroDeSubstratosInicial == 700 & planilha$NumeroDePredadoresInicial == 150 & planilha$NumeroDaGeracao <= 20 & planilha$NumeroDoExperimento == 30,]


substratoInicial50PredadoresAteGeracao20 <- c(substrato300E50DePredadoresInicialExp1AteGeracao20$NumeroDeSubstratosInicial, substrato300E50DePredadoresInicialExp2AteGeracao20$NumeroDeSubstratosInicial, substrato300E50DePredadoresInicialExp3AteGeracao20$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp4AteGeracao20$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp5AteGeracao20$NumeroDeSubstratosInicial, substrato400E50DePredadoresInicialExp6AteGeracao20$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp7AteGeracao20$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp8AteGeracao20$NumeroDeSubstratosInicial, substrato500E50DePredadoresInicialExp9AteGeracao20$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp10AteGeracao20$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp11AteGeracao20$NumeroDeSubstratosInicial, substrato600E50DePredadoresInicialExp12AteGeracao20$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp13AteGeracao20$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp14AteGeracao20$NumeroDeSubstratosInicial, substrato700E50DePredadoresInicialExp15AteGeracao20$NumeroDeSubstratosInicial)
substratoInicial50PredadoresAteGeracao20 <- as.factor(substratoInicial50PredadoresAteGeracao20)

predadoresVivosAteGeracao20 <- c(substrato300E50DePredadoresInicialExp1AteGeracao20$NumeroDePredadoresVivos, substrato300E50DePredadoresInicialExp2AteGeracao20$NumeroDePredadoresVivos, substrato300E50DePredadoresInicialExp3AteGeracao20$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp4AteGeracao20$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp5AteGeracao20$NumeroDePredadoresVivos, substrato400E50DePredadoresInicialExp6AteGeracao20$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp7AteGeracao20$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp8AteGeracao20$NumeroDePredadoresVivos, substrato500E50DePredadoresInicialExp9AteGeracao20$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp10AteGeracao20$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp11AteGeracao20$NumeroDePredadoresVivos, substrato600E50DePredadoresInicialExp12AteGeracao20$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp13AteGeracao20$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp14AteGeracao20$NumeroDePredadoresVivos, substrato700E50DePredadoresInicialExp15AteGeracao20$NumeroDePredadoresVivos)

png("prop1_3SemZero.png")
boxplot(predadoresVivosAteGeracao20 ~ substratoInicial50PredadoresAteGeracao20, data = planilha, main = "Predadores Vivos x Substrato Inicial", xlab = "Substrato Inicial", ylab = "Predadores Vivos", col = "#f55435")
dev.off()
planilhaPergunta1AteGeracao20 <- data.frame(substratoInicial50PredadoresAteGeracao20, predadoresVivosAteGeracao20)

# Teste de Homogeneidade das Variancias [OK]
leveneTest(planilhaPergunta1AteGeracao20$predadoresVivosAteGeracao20 ~ planilhaPergunta1AteGeracao20$substratoInicial50PredadoresAteGeracao20) # p = 0.9

# Teste de Normalidade
resultadoANOVAAteGeracao20 <- aov(predadoresVivosAteGeracao20 ~ substratoInicial50PredadoresAteGeracao20, data = planilhaPergunta1AteGeracao20)

resultado2ANOVAAteGeracao20 <- lm(predadoresVivosAteGeracao20 ~ substratoInicial50PredadoresAteGeracao20, data = planilhaPergunta1AteGeracao20)

summary(resultadoANOVAAteGeracao20)
summary(resultado2ANOVAAteGeracao20)

testeNaoParametricoAteGeracao20 <- kruskal.test(predadoresVivosAteGeracao20 ~ substratoInicial50PredadoresAteGeracao20, data = planilhaPergunta1AteGeracao20)
testeNaoParametricoAteGeracao20


# Repetindo para 150 predadores

substratoInicial150PredadoresAteGeracao20 <- c(substrato300E150DePredadoresInicialExp16AteGeracao20$NumeroDeSubstratosInicial, substrato300E150DePredadoresInicialExp17AteGeracao20$NumeroDeSubstratosInicial, substrato300E150DePredadoresInicialExp18AteGeracao20$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp19AteGeracao20$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp20AteGeracao20$NumeroDeSubstratosInicial, substrato400E150DePredadoresInicialExp21AteGeracao20$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp22AteGeracao20$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp23AteGeracao20$NumeroDeSubstratosInicial, substrato500E150DePredadoresInicialExp24AteGeracao20$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp25AteGeracao20$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp26AteGeracao20$NumeroDeSubstratosInicial, substrato600E150DePredadoresInicialExp27AteGeracao20$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp28AteGeracao20$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp29AteGeracao20$NumeroDeSubstratosInicial, substrato700E150DePredadoresInicialExp30AteGeracao20$NumeroDeSubstratosInicial)
substratoInicial150PredadoresAteGeracao20 <- as.factor(substratoInicial150PredadoresAteGeracao20)

predadoresVivos150AteGeracao20 <- c(substrato300E150DePredadoresInicialExp16AteGeracao20$NumeroDePredadoresVivos, substrato300E150DePredadoresInicialExp17AteGeracao20$NumeroDePredadoresVivos, substrato300E150DePredadoresInicialExp18AteGeracao20$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp19AteGeracao20$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp20AteGeracao20$NumeroDePredadoresVivos, substrato400E150DePredadoresInicialExp21AteGeracao20$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp22AteGeracao20$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp23AteGeracao20$NumeroDePredadoresVivos, substrato500E150DePredadoresInicialExp24AteGeracao20$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp25AteGeracao20$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp26AteGeracao20$NumeroDePredadoresVivos, substrato600E150DePredadoresInicialExp27AteGeracao20$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp28AteGeracao20$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp29AteGeracao20$NumeroDePredadoresVivos, substrato700E150DePredadoresInicialExp30AteGeracao20$NumeroDePredadoresVivos)

png("prop3_1SemZero.png")
boxplot(predadoresVivos150AteGeracao20 ~ substratoInicial150PredadoresAteGeracao20, data = planilha, main = "Predadores Vivos x Substrato Inicial", xlab = "Substrato Inicial", ylab = "Predadores Vivos", col = "#f55435")
dev.off()
planilhaPergunta1150AteGeracao20 <- data.frame(substratoInicial150PredadoresAteGeracao20, predadoresVivos150AteGeracao20)

# Teste de Homogeneidade das Variancias [OK]
leveneTest(planilhaPergunta1150AteGeracao20$predadoresVivos150AteGeracao20 ~ planilhaPergunta1150AteGeracao20$substratoInicial150PredadoresAteGeracao20) # p = 0.9

# Teste de Normalidade
resultadoANOVA150AteGeracao20 <- aov(predadoresVivos150AteGeracao20 ~ substratoInicial150PredadoresAteGeracao20, data = planilhaPergunta1150AteGeracao20)

resultado2ANOVA150AteGeracao20 <- lm(predadoresVivos150AteGeracao20 ~ substratoInicial150PredadoresAteGeracao20, data = planilhaPergunta1150AteGeracao20)

summary(resultadoANOVA150AteGeracao20)
summary(resultado2ANOVA150AteGeracao20)

testeNaoParametrico150AteGeracao20 <- kruskal.test(predadoresVivos150AteGeracao20 ~ substratoInicial150PredadoresAteGeracao20, data = planilhaPergunta1150AteGeracao20)
testeNaoParametrico150AteGeracao20

# Resposta: nao faz diferenca significativa o aumento de substrato para retardar o tempo de extincao dos predadores