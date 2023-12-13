
@Title: Simulacao de Ambiente com Levantamento de Dados
@Author: Ramon M. Teixeira
@Date: 2023-12-03
@Version: 1.0

##### Enunciado #####

# Presa -> Quadrados de cor azul
# Predador -> Quadrados de cor vermelha
# Recursos -> Quadrados de cor verde

# Presa tem a estrategia R -> reproducao rapida e vida curta
# Predador tem a estrategia K -> reproducao lenta e vida longa

# Apenas as presas se alimentam do substrato
# Apenas os predadores se alimentam das presas
# Todos precisam se alimentar antes da reproducao
# A alimentacao ocorre se o alimento (presa ou substrato) estiver no campo adjascente

# Predador e Presa morrem de fome de acordo com suas configuracoes especificas de fome
# Predador e Presa morrem se o tempo de vida terminar

# A reproducao so ocorre a cada configuracao especifica de tempo
# A reproducao pode ocorrer em cima de um substrato ou campo vazio apenas
# A reproducao ocorre em um campo adjascente aleatorio
# Cada reproducao geram dois descendentes

# Apenas os predadores se locomovem

##### Legenda do Mapa #####

# 0 -> Espaço Vazio
# 1-> Substrato
# 2 -> Presa
# 3 -> Predador

##### Coleta e Armazenamento dos Dados da Simulacao #####

### Parametros Fixos (SUGESTAO):

# Area Total = 30 x 30 (900)
# Numero de Geracoes = 50

# Tempo de Vida da Presa = 4
# Tempo de Vida do Predador = 12

# Tempo de Morte da Presa com Fome = 2
# Tempo de Morte do Predador com Fome = 6

# Tempo de Reproducao da Presa = 3
# Tempo de Reproducao do Predador = 7

# Configurando armazenamento

### PRIMEIRA VEZ RODANDO O CODIGO
# Necessario criar uma base de dados inicial vazia e consumi-la

# registro <- data.frame(
#     NumeroDoExperimento = 0,
#     NumeroDaGeracao = 0,
#     NumeroDeEspacosVazios = 0,
#     NumeroDeSubstratosInicial = 0,
#     NumeroDePresasInicial = 0,
#     NumeroDePredadoresInicial = 0,
#     NumeroDePresasReproduzidas = 0,
#     NumeroDePredadoresReproduzidos = 0,
#     NumeroDePresasMortasDeFome = 0,
#     NumeroDePredadoresMortosDeFome = 0,
#     NumeroDePresasMortasPorIdade = 0,
#     NumeroDePredadoresMortosPorIdade = 0,
#     NumeroDePresasMortasPorPredadores = 0,
#     NumeroDeSubstratosConsumidos = 0,
#     NumeroDeSubstratoRestante = 0,
#     NumeroDePresasVivas = 0,
#     NumeroDePredadoresVivos = 0
# )

# write.csv(registro, "planilha.csv", row.names = FALSE)

# Apos isso, comente o codigo acima e rode o codigo inteiro abaixo
planilha <- read.csv("planilha.csv")

registro <- data.frame(
    NumeroDoExperimento = 0,
    NumeroDaGeracao = 0,
    NumeroDeEspacosVazios = 0,
    NumeroDeSubstratosInicial = 0,
    NumeroDePresasInicial = 0,
    NumeroDePredadoresInicial = 0,
    NumeroDePresasReproduzidas = 0,
    NumeroDePredadoresReproduzidos = 0,
    NumeroDePresasMortasDeFome = 0,
    NumeroDePredadoresMortosDeFome = 0,
    NumeroDePresasMortasPorIdade = 0,
    NumeroDePredadoresMortosPorIdade = 0,
    NumeroDePresasMortasPorPredadores = 0,
    NumeroDeSubstratosConsumidos = 0,
    NumeroDeSubstratoRestante = 0,
    NumeroDePresasVivas = 0,
    NumeroDePredadoresVivos = 0
)
#lista de registros
planilha <<- data.frame(planilha)

##### Inputs #####

# Experimento
# Mudar a cada armazenamento
numeroDoExperimento <- 30

# Area do Mapa
linhasTotais <- 30
colunasTotais <- 30
numeroDeGeracoes <- 50

# Elementos do Mapa
quantidadeDePresas <- 50
quantidadeDePredadores <- 150
quantidadeDeSubstrato <- 700

# Parametros dos Elementos
# Tempo de Morte com Fome deve ser menor que o Tempo de Demora Para a Reproducao
tempoDeVidaDaPresa <- 4
tempoDeVidaDoPredador <- 12

tempoDeMorteDaPresaComFome <- 2
tempoDeMorteDoPredadorComFome <- 6

tempoDeReproducaoDaPresa <- 3
tempoDeReproducaoDoPredador <- 7

# Entidades
Entidade <- list(
  tipo = 0,   
  tempoRestanteParaMorrerDeFome = 0,        
  tempoRestanteDeVida = 0, 
  tempoParaProximaReproducao = 0   
)

# Registrando parametros iniciais
registro$NumeroDoExperimento <- numeroDoExperimento
registro$NumeroDeSubstratosInicial <- quantidadeDeSubstrato
registro$NumeroDePresasInicial <- quantidadeDePresas
registro$NumeroDePredadoresInicial <- quantidadeDePredadores

# Criando local para registro dos parametros que sofrem mudanca com as geracoes
numeroDaGeracao <- 1
numeroDeEspacosVazios <- 0
numeroDePresasReproduzidas <- 0
numeroDePredadoresReproduzidos <- 0
numeroDePresasMortasDeFome <- 0
numeroDePredadoresMortosDeFome <- 0
numeroDePresasMortasPorIdade <- 0
numeroDePredadoresMortosPorIdade <- 0
numeroDePresasMortasPorPredadores <- 0
numeroDeSubstratosConsumidos <- 0
numeroDeSubstratosRestantes <- 0
numeroDePresasVivas <- 0
numeroDePredadoresVivos <- 0

##### Funcoes #####

library(animation)

criarMapaInicial <- function() {
    
    # Criando mapa vazio
    mapa <- matrix(list(), nrow = linhasTotais, ncol = colunasTotais)

    EntidadeVazia <- Entidade

    # Preenchendo mapa com espacos vazios
    for (i in 1:linhasTotais) {
        for (j in 1:colunasTotais) {
            mapa[[i, j]] <- EntidadeVazia
        }
    }

    # Inserindo substrato (Apenas se a quantidade for compativel com o tamanho do mapa)
    if(quantidadeDeSubstrato < (linhasTotais * colunasTotais)){
        for(i in 1:quantidadeDeSubstrato){
            linha <- sample(1:linhasTotais, 1)
            coluna <- sample(1:colunasTotais, 1)
            mapa[[linha, coluna]][[1]][1] <- 1

            numeroDeSubstratosRestantes <- numeroDeSubstratosRestantes + 1
        }
    } else {
        print("Quantidade de substrato incompativel com o tamanho do mapa")
    }

    # Inserindo presas (Apenas se a quantidade for compativel com o tamanho do mapa)
    
    # Para isso, vamos precisar saber quantos espacos vazios ainda restam
    espacosVazios <- 0
    for (i in 1:linhasTotais) {
        for (j in 1:colunasTotais) {
            if(mapa[[i, j]][[1]][1] == 0){
                espacosVazios <- espacosVazios + 1

                numeroDeEspacosVazios <- numeroDeEspacosVazios + 1
            }
        }
    }
    if(quantidadeDePresas < espacosVazios){
        for(i in 1:quantidadeDePresas){
            linha <- sample(1:linhasTotais, 1)
            coluna <- sample(1:colunasTotais, 1)
            while(mapa[[linha, coluna]][[1]][1] != 1){
                linha <- sample(1:linhasTotais, 1)
                coluna <- sample(1:colunasTotais, 1)
            }
            mapa[[linha, coluna]][[1]][1] <- 2
            mapa[[linha, coluna]][[2]][1] <- tempoDeMorteDaPresaComFome
            mapa[[linha, coluna]][[3]][1] <- tempoDeVidaDaPresa
            mapa[[linha, coluna]][[4]][1] <- tempoDeReproducaoDaPresa

            numeroDePresasVivas <- numeroDePresasVivas + 1
        }
    } else {
        print("Quantidade de presas incompativel com o numero de espacos vazios no mapa")
    }

    # Inserindo predadores (Apenas se a quantidade for compativel com o tamanho do mapa)

    # Para isso, vamos precisar saber quantos espacos vazios ainda restam
    espacosVazios <- 0
    for (i in 1:linhasTotais) {
        for (j in 1:colunasTotais) {
            if(mapa[[i, j]][[1]][1] == 0){
                espacosVazios <- espacosVazios + 1
            }
        }
    }
    if(quantidadeDePredadores < espacosVazios){
        for(i in 1:quantidadeDePredadores){
            linha <- sample(1:linhasTotais, 1)
            coluna <- sample(1:colunasTotais, 1)
            while(mapa[[linha, coluna]][[1]][1] != 1 && mapa[[linha, coluna]][[1]][1] != 2){
                linha <- sample(1:linhasTotais, 1)
                coluna <- sample(1:colunasTotais, 1)
            }
            mapa[[linha, coluna]][[1]][1] <- 3
            mapa[[linha, coluna]][[2]][1] <- tempoDeMorteDoPredadorComFome
            mapa[[linha, coluna]][[3]][1] <- tempoDeVidaDoPredador
            mapa[[linha, coluna]][[4]][1] <- tempoDeReproducaoDoPredador

            numeroDePredadoresVivos <- numeroDePredadoresVivos + 1
        }
    } else {
        print("Quantidade de predadores incompativel com o numero de espacos vazios no mapa")
    }

    # Armazenando mapa inicial
    registro$NumeroDoExperimento <- numeroDoExperimento
    registro$NumeroDaGeracao <- numeroDaGeracao
    registro$NumeroDeEspacosVazios <- numeroDeEspacosVazios
    registro$NumeroDeSubstratosInicial <- quantidadeDeSubstrato
    registro$NumeroDePresasInicial <- quantidadeDePresas
    registro$NumeroDePredadoresInicial <- quantidadeDePredadores
    registro$NumeroDePresasReproduzidas <- numeroDePresasReproduzidas
    registro$NumeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos
    registro$NumeroDePresasMortasDeFome <- numeroDePresasMortasDeFome
    registro$NumeroDePredadoresMortosDeFome <- numeroDePredadoresMortosDeFome
    registro$NumeroDePresasMortasPorIdade <- numeroDePresasMortasPorIdade
    registro$NumeroDePredadoresMortosPorIdade <- numeroDePredadoresMortosPorIdade
    registro$NumeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores
    registro$NumeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos
    registro$NumeroDeSubstratoRestante <- numeroDeSubstratosRestantes
    registro$NumeroDePresasVivas <- numeroDePresasVivas
    registro$NumeroDePredadoresVivos <- numeroDePredadoresVivos

    planilha <<- rbind(planilha, registro)


    return (mapa)
}

proximaGeracao <- function(mapa) {

    numeroDaGeracao <<- numeroDaGeracao + 1
    novaGeracao <- mapa

    for(i in 1:nrow(mapa)){
        for(j in 1:ncol(mapa)){
            novaGeracao[[i, j]][[2]][1] <- mapa[[i, j]][[2]][1]
            novaGeracao[[i, j]][[3]][1] <- mapa[[i, j]][[3]][1]
            novaGeracao[[i, j]][[4]][1] <- mapa[[i, j]][[4]][1] 

            print(novaGeracao[[i, j]][[2]][1])
        }
    }
    
    #### Regras de Morte ####

    # Morre se estiver com fome ou sem tempo de vida
    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){
            if(novaGeracao[[i, j]][[1]][1] != 0 && novaGeracao[[i, j]][[1]][1] != 1){
                if((novaGeracao[[i, j]][[1]][1] == 2 || novaGeracao[[i, j]][[1]][1] == 3) && novaGeracao[[i, j]][[2]][1] <= 0){
                    novaGeracao[[i, j]][[1]][1] <- 0

                    if(novaGeracao[[i, j]][[1]][1] == 2){
                        numeroDePresasMortasDeFome <- numeroDePresasMortasDeFome + 1
                    } else {
                        numeroDePredadoresMortosDeFome <- numeroDePredadoresMortosDeFome + 1
                    }

                } 
                else if((novaGeracao[[i, j]][[1]][1] == 2 || novaGeracao[[i, j]][[1]][1] == 3 ) && novaGeracao[[i, j]][[3]][1] <= 0){
                    novaGeracao[[i, j]][[1]][1] <- 0

                    if(novaGeracao[[i, j]][[1]][1] == 2){
                        numeroDePresasMortasPorIdade <- numeroDePresasMortasPorIdade + 1
                    } else {
                        numeroDePredadoresMortosPorIdade <- numeroDePredadoresMortosPorIdade + 1
                    }
                }
            }

            if(novaGeracao[[i, j]][[1]][1] == 0){
                numeroDeEspacosVazios <- numeroDeEspacosVazios + 1
            } else if(novaGeracao[[i, j]][[1]][1] == 2){
                numeroDePresasVivas <- numeroDePresasVivas + 1
            } else if(novaGeracao[[i, j]][[1]][1] == 3){
                numeroDePredadoresVivos <- numeroDePredadoresVivos + 1
            } else if(novaGeracao[[i, j]][[1]][1] == 1){
                numeroDeSubstratosRestantes <- numeroDeSubstratosRestantes + 1
            }
        }
    }

    #### Regras de Alimentacao ####

    #Presa come substrato ao redor caso exista
    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){
            if(novaGeracao[[i, j]][[1]][1] == 2){
                if(i > 1 && j > 1 && i < nrow(novaGeracao) && j < ncol(novaGeracao)){
                    if(i > 2 && novaGeracao[[i - 1, j]][[1]][1] == 1){
                        novaGeracao[[i - 1, j]][[1]][1] <- 0
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDaPresaComFome

                        numeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos + 1
                    } 
                    else if(i < (linhasTotais - 1) && novaGeracao[[i + 1, j]][[1]][1] == 1){
                        novaGeracao[[i + 1, j]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDaPresaComFome

                        numeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos + 1
                    }
                    else if(j > 1 && novaGeracao[[i, j - 1]][[1]][1] == 1){
                        novaGeracao[[i, j - 1]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDaPresaComFome

                        numeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos + 1
                    }
                    else if(j < (colunasTotais - 1) && novaGeracao[[i, j + 1]][[1]][1] == 1){
                        novaGeracao[[i, j + 1]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDaPresaComFome

                        numeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos + 1
                    }  
                }
            }
        }
    }

    # Predador come presa ao redor caso exista e se faltar metade do tempo para morrer de fome
    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){
            if(novaGeracao[[i, j]][[1]][1] == 3 && novaGeracao[[i, j]][[2]][1] <= tempoDeMorteDoPredadorComFome / 2){
                if(i > 1 && j > 1 && i < nrow(novaGeracao) && j < ncol(novaGeracao)){
                    if(i > 2 && novaGeracao[[i - 1, j]][[1]][1] == 2){
                        novaGeracao[[i - 1, j]][[1]][1] <- 0
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDoPredadorComFome

                        numeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores + 1
                    } 
                    else if(i < (linhasTotais - 1) && novaGeracao[[i + 1, j]][[1]][1] == 2){
                        novaGeracao[[i + 1, j]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDoPredadorComFome

                        numeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores + 1
                    }
                    else if(j > 1 && novaGeracao[[i, j - 1]][[1]][1] == 2){
                        novaGeracao[[i, j - 1]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDoPredadorComFome

                        numeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores + 1
                    }
                    else if(j < (colunasTotais - 1) && novaGeracao[[i, j + 1]][[1]][1] == 2){
                        novaGeracao[[i, j + 1]][[1]][1] <- 0 
                        novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] + tempoDeMorteDoPredadorComFome

                        numeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores + 1
                    }  
                }
            }
        }
    }

    #### Regras de Reproducao ####

    # Ambos se reproduzem e o contador volta ao default

    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){

            # PRESAS SE REPRODUZEM
            if(novaGeracao[[i, j]][[1]][1] == 2 && novaGeracao[[i, j]][[2]][1] > 0){

                if(novaGeracao[[i, j]][[4]][1] <= 0){
                    
                    numeroDeReproducoes <- 0

                    # Reproduz duas vezes
                    while(numeroDeReproducoes < 2){

                        espacoAmostralDeLocaisParaReproduzir <- c()
                        if(i > 1 && j > 1 && i < nrow(novaGeracao) && j < ncol(novaGeracao)){
                            if (i > 2 && (novaGeracao[[i - 1, j]][[1]][1] == 0 || novaGeracao[[i - 1, j]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 1)
                            }
                            if (i < (linhasTotais - 1) && (novaGeracao[[i + 1, j]][[1]][1] == 0 || novaGeracao[[i + 1, j]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 2)
                            }
                            if (j > 2 && (novaGeracao[[i, j - 1]][[1]][1] == 0 || novaGeracao[[i, j - 1]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 3)
                            }
                            if (j < (colunasTotais - 1) && (novaGeracao[[i, j + 1]][[1]][1] == 0 || novaGeracao[[i, j + 1]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 4)
                            }
                            print(espacoAmostralDeLocaisParaReproduzir)

                            # Verifica se há elementos na lista antes de chamar sample
                            if (length(espacoAmostralDeLocaisParaReproduzir) > 0) {
                                ondeReproduzir <- sample(espacoAmostralDeLocaisParaReproduzir, 1)
                            } else {
                                print("Lista vazia, nenhum elemento para amostragem.")
                            }
                        

                            # Realizando a reproducao

                            if(ondeReproduzir == 1){
                                novaGeracao[[i - 1, j]][[1]][1] <- 2
                                novaGeracao[[i - 1, j]][[2]][1] <- tempoDeMorteDaPresaComFome
                                novaGeracao[[i - 1, j]][[3]][1] <- tempoDeVidaDaPresa
                                novaGeracao[[i - 1, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                numeroDePresasReproduzidas <- numeroDePresasReproduzidas + 1
                            } 
                            else if(ondeReproduzir == 2){
                                novaGeracao[[i + 1, j]][[1]][1] <- 2
                                novaGeracao[[i + 1, j]][[2]][1] <- tempoDeMorteDaPresaComFome
                                novaGeracao[[i + 1, j]][[3]][1] <- tempoDeVidaDaPresa
                                novaGeracao[[i + 1, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                numeroDePresasReproduzidas <- numeroDePresasReproduzidas + 1
                            }
                            else if(ondeReproduzir == 3){
                                novaGeracao[[i, j - 1]][[1]][1] <- 2
                                novaGeracao[[i, j - 1]][[2]][1] <- tempoDeMorteDaPresaComFome
                                novaGeracao[[i, j - 1]][[3]][1] <- tempoDeVidaDaPresa
                                novaGeracao[[i, j - 1]][[4]][1] <- tempoDeReproducaoDaPresa

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                numeroDePresasReproduzidas <- numeroDePresasReproduzidas + 1
                            }
                            else if(ondeReproduzir == 4){
                                novaGeracao[[i, j + 1]][[1]][1] <- 2
                                novaGeracao[[i, j + 1]][[2]][1] <- tempoDeMorteDaPresaComFome
                                novaGeracao[[i, j + 1]][[3]][1] <- tempoDeVidaDaPresa
                                novaGeracao[[i, j + 1]][[4]][1] <- tempoDeReproducaoDaPresa

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDaPresa

                                numeroDePresasReproduzidas <- numeroDePresasReproduzidas + 1
                            } 

                            numeroDeReproducoes <- numeroDeReproducoes + 1
                        }
                    }
                } 
            }

            # # PREDADORES SE REPRODUZEM
            if(novaGeracao[[i, j]][[1]][1] == 3 && novaGeracao[[i, j]][[2]][1] > 0){

                if(novaGeracao[[i, j]][[4]][1] <= 0){
                    
                    numeroDeReproducoes <- 0

                    # Reproduz duas vezes
                    while(numeroDeReproducoes < 2){

                        espacoAmostralDeLocaisParaReproduzir <- c()

                        if(i > 1 && j > 1 && i < nrow(novaGeracao) && j < ncol(novaGeracao)){
                            if (i > 2 && (novaGeracao[[i - 1, j]][[1]][1] == 0 || novaGeracao[[i - 1, j]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 1)
                            }
                            if (i < (linhasTotais - 1) && (novaGeracao[[i + 1, j]][[1]][1] == 0 || novaGeracao[[i + 1, j]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 2)
                            }
                            if (j > 2 && (novaGeracao[[i, j - 1]][[1]][1] == 0 || novaGeracao[[i, j - 1]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 3)
                            }
                            if (j < (colunasTotais - 1) && (novaGeracao[[i, j + 1]][[1]][1] == 0 || novaGeracao[[i, j + 1]][[1]][1] == 1)) {
                                # Adiciona na lista de possíveis locais para reproduzir
                                espacoAmostralDeLocaisParaReproduzir <- c(espacoAmostralDeLocaisParaReproduzir, 4)
                            }

                            # Verifica se há elementos na lista antes de chamar sample
                            if (length(espacoAmostralDeLocaisParaReproduzir) > 0) {
                                ondeReproduzir <- sample(espacoAmostralDeLocaisParaReproduzir, 1)
                            } else {
                                print("Lista vazia, nenhum elemento para amostragem.")
                            }
                        

                            # Realizando a reproducao
                            if(ondeReproduzir == 1){
                                novaGeracao[[i - 1, j]][[1]][1] <- 3
                                novaGeracao[[i - 1, j]][[2]][1] <- tempoDeMorteDoPredadorComFome
                                novaGeracao[[i - 1, j]][[3]][1] <- tempoDeVidaDoPredador
                                novaGeracao[[i - 1, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                numeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos + 1
                            } 
                            else if(ondeReproduzir == 2){
                                novaGeracao[[i + 1, j]][[1]][1] <- 3
                                novaGeracao[[i + 1, j]][[2]][1] <- tempoDeMorteDoPredadorComFome
                                novaGeracao[[i + 1, j]][[3]][1] <- tempoDeVidaDoPredador
                                novaGeracao[[i + 1, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                numeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos + 1
                            }
                            else if(ondeReproduzir == 3){
                                novaGeracao[[i, j - 1]][[1]][1] <- 3
                                novaGeracao[[i, j - 1]][[2]][1] <- tempoDeMorteDoPredadorComFome
                                novaGeracao[[i, j - 1]][[3]][1] <- tempoDeVidaDoPredador
                                novaGeracao[[i, j - 1]][[4]][1] <- tempoDeReproducaoDoPredador

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                numeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos + 1
                            }
                            else if(ondeReproduzir == 4){
                                novaGeracao[[i, j + 1]][[1]][1] <- 3
                                novaGeracao[[i, j + 1]][[2]][1] <- tempoDeMorteDoPredadorComFome
                                novaGeracao[[i, j + 1]][[3]][1] <- tempoDeVidaDoPredador
                                novaGeracao[[i, j + 1]][[4]][1] <- tempoDeReproducaoDoPredador

                                novaGeracao[[i, j]][[4]][1] <- tempoDeReproducaoDoPredador

                                numeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos + 1
                            }

                            numeroDeReproducoes <- numeroDeReproducoes + 1
                        }
                    }
                }
            }
        }
    }
    
    #### Regras de Locomocao ####
    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){

            # PREDADORES SE LOCOMOVEM
            if(novaGeracao[[i, j]][[1]][1] == 3 && novaGeracao[[i, j]][[2]][1] > 0){

                espacoAmostralDeLocaisParaLocomover <- c()
                if(i > 1 && j > 1 && i < nrow(novaGeracao) && j < ncol(novaGeracao)){
                    if (i > 2 && (novaGeracao[[i - 1, j]][[1]][1] == 0 || novaGeracao[[i - 1, j]][[1]][1] == 1)) {
                        # Adiciona na lista de possíveis locais para locomover
                        espacoAmostralDeLocaisParaLocomover <- c(espacoAmostralDeLocaisParaLocomover, 1)
                    }
                    if (i < (linhasTotais - 4) && (novaGeracao[[i + 1, j]][[1]][1] == 0 || novaGeracao[[i + 1, j]][[1]][1] == 1)) {
                        # Adiciona na lista de possíveis locais para locomover
                        espacoAmostralDeLocaisParaLocomover <- c(espacoAmostralDeLocaisParaLocomover, 2)
                    }
                    if (j > 2 && (novaGeracao[[i, j - 1]][[1]][1] == 0 || novaGeracao[[i, j - 1]][[1]][1] == 1)) {
                        # Adiciona na lista de possíveis locais para locomover
                        espacoAmostralDeLocaisParaLocomover <- c(espacoAmostralDeLocaisParaLocomover, 3)
                    }
                    if (j < (colunasTotais - 4) && (novaGeracao[[i, j + 1]][[1]][1] == 0 || novaGeracao[[i, j + 1]][[1]][1] == 1)) {
                        # Adiciona na lista de possíveis locais para locomover
                        espacoAmostralDeLocaisParaLocomover <- c(espacoAmostralDeLocaisParaLocomover, 4)
                    }

                    # Verifica se há elementos na lista antes de chamar sample
                    if (length(espacoAmostralDeLocaisParaLocomover ) > 0) {
                        ondeLocomover <- sample(espacoAmostralDeLocaisParaLocomover , 1)
                    } else {
                        print("Lista vazia, nenhum elemento para amostragem.")
                    }

                    # Realizando a locomocao sem romper indices
                    if(ondeLocomover == 1){ 
                        novaGeracao[[i - 1, j]][[1]][1] <- 3
                        novaGeracao[[i - 1, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] - 1
                        novaGeracao[[i - 1, j]][[3]][1] <- novaGeracao[[i,j]][[3]][1] - 1
                        novaGeracao[[i - 1, j]][[4]][1] <- novaGeracao[[i,j]][[4]][1] - 1

                        novaGeracao[[i, j]][[1]][1] <- 0
                    } 
                    else if(ondeLocomover == 2){
                        novaGeracao[[i + 1, j]][[1]][1] <- 3
                        novaGeracao[[i + 1, j]][[2]][1] <- novaGeracao[[i,j]][[2]][1] - 1
                        novaGeracao[[i + 1, j]][[3]][1] <- novaGeracao[[i,j]][[3]][1] - 1
                        novaGeracao[[i + 1, j]][[4]][1] <- novaGeracao[[i,j]][[4]][1] - 1

                        novaGeracao[[i, j]][[1]][1] <- 0
                    }
                    else if(ondeLocomover == 3){
                        novaGeracao[[i, j - 1]][[1]][1] <- 3
                        novaGeracao[[i, j - 1]][[2]][1] <- novaGeracao[[i,j]][[2]][1] - 1
                        novaGeracao[[i, j - 1]][[3]][1] <- novaGeracao[[i,j]][[3]][1] - 1
                        novaGeracao[[i, j - 1]][[4]][1] <- novaGeracao[[i,j]][[4]][1] - 1

                        novaGeracao[[i, j]][[1]][1] <- 0
                    }
                    else if(ondeLocomover == 4){
                        novaGeracao[[i, j + 1]][[1]][1] <- 3
                        novaGeracao[[i, j + 1]][[2]][1] <- novaGeracao[[i,j]][[2]][1] - 1
                        novaGeracao[[i, j + 1]][[3]][1] <- novaGeracao[[i,j]][[3]][1] - 1
                        novaGeracao[[i, j + 1]][[4]][1] <- novaGeracao[[i,j]][[4]][1] - 1

                        novaGeracao[[i, j]][[1]][1] <- 0
                    }
                }
            }
        }
    }

    #### SE AINDA ESTA VIVO, ATUALIZA OS CONTADORES ####
    for(i in 1:nrow(novaGeracao)){
        for(j in 1:ncol(novaGeracao)){
            if(novaGeracao[[i, j]][[1]][1] == 2 || novaGeracao[[i, j]][[1]][1] == 3){
                novaGeracao[[i, j]][[2]][1] <- novaGeracao[[i, j]][[2]][1] - 1
                novaGeracao[[i, j]][[3]][1] <- novaGeracao[[i, j]][[3]][1] - 1
                novaGeracao[[i, j]][[4]][1] <- novaGeracao[[i, j]][[4]][1] - 1
            }
        }
    }

    registro$NumeroDoExperimento <- numeroDoExperimento
    registro$NumeroDaGeracao <- numeroDaGeracao
    registro$NumeroDeEspacosVazios <- numeroDeEspacosVazios
    registro$NumeroDeSubstratosInicial <- quantidadeDeSubstrato
    registro$NumeroDePresasInicial <- quantidadeDePresas
    registro$NumeroDePredadoresInicial <- quantidadeDePredadores
    registro$NumeroDePresasReproduzidas <- numeroDePresasReproduzidas
    registro$NumeroDePredadoresReproduzidos <- numeroDePredadoresReproduzidos
    registro$NumeroDePresasMortasDeFome <- numeroDePresasMortasDeFome
    registro$NumeroDePredadoresMortosDeFome <- numeroDePredadoresMortosDeFome
    registro$NumeroDePresasMortasPorIdade <- numeroDePresasMortasPorIdade
    registro$NumeroDePredadoresMortosPorIdade <- numeroDePredadoresMortosPorIdade
    registro$NumeroDePresasMortasPorPredadores <- numeroDePresasMortasPorPredadores
    registro$NumeroDeSubstratosConsumidos <- numeroDeSubstratosConsumidos
    registro$NumeroDeSubstratoRestante <- numeroDeSubstratosRestantes
    registro$NumeroDePresasVivas <- numeroDePresasVivas
    registro$NumeroDePredadoresVivos <- numeroDePredadoresVivos

    planilha <<- rbind(planilha, registro)

    return (novaGeracao)
}

desenharMapa <- function(mapa) {

    desenho <- plot(0, type = "n", xlim = c(0, ncol(mapa)), ylim = c(0, nrow(mapa)), xlab = "", ylab = "")

    for (i in 1:linhasTotais) {
        for (j in 1:colunasTotais) {
            if(mapa[[i, j]][[1]][1] == 0){
                rect(i, j, i + 1, j + 1, col = "white")
            } 
            else if(mapa[[i, j]][[1]][1] == 1){
                rect(i, j, i + 1, j + 1, col = "green")
            } 
            else if(mapa[[i, j]][[1]][1] == 2){
                rect(i, j, i + 1, j + 1, col = "blue")
            } 
            else if(mapa[[i, j]][[1]][1] == 3){
                rect(i, j, i + 1, j + 1, col = "red")
            }
        }
    }
}

atualizarAnimacao <- function(mapa, geracao) {
  mapa <<- proximaGeracao(mapa)
  desenharMapa(mapa)

  title(paste("Geração:", geracao), line = 2, cex.main = 2.5)
}

##### CRIANDO SIMULACAO #####

mapa <- criarMapaInicial()

# Criando a animação
ani.options(interval = 0.2)
saveGIF({
  for (geracao in 1:numeroDeGeracoes) {
    atualizarAnimacao(mapa, geracao)
  }
}, movie.name = "Exp30.gif", ani.width = 1000, ani.height = 1000)

write.csv(planilha, file = "planilha.csv", row.names = FALSE)

