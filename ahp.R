# instalações de pacotes --------------------------------------------------
# se já tiver este pacote, não precisa instalar.
install.packages("rgdal")

# abrindo pacote necessário------------------------------------------------

library(rgdal)

# abrindo o shapefile com os dados ----------------------------------------

dados.ahp <- readOGR("grade_celular.shp") # colocar o caminho onde está seu shapefile de interesse
View(dados.ahp@data) # visualizar a tabela de dados
str(dados.ahp@data) # outra forma de visualizar quais são as colunas e os tipos das variáveis

# Os dados a partir da variável 'V80' foram criados com a simulação que será feita ainda nesse script.
# Portanto se quiser apagá-los, é só usar os camandos a baixo.
drop = c(names(dados.ahp@data[,c(80:length(names(dados.ahp)))]))
dados.ahp <- dados.ahp[,!(names(dados) %in% drop)]

# AHP ---------------------------------------------------------------------

# A variável 'ahp' já está criada em 'dados.ahp', por isso não precisa fazer esse passo.
# Mas deixei aqui para mostrar como criá-la.

# atribuindo os pesos determinado na AHP (calculados na tabela AHP 6.0.xlsx) que multiplicará cada variável
# cada peso será usado para mutiplicar uma variável identificada no comentário ao lado
p1 = 0.2019 #Area Aberta
p2 = 0.1464 #Vegeta??o Secund?ria
p3 = 0.1464 #BR 163
p4 = 0.0984 #Proximidade a ?rea urbana
p5 = 0.0859 #Estradas vicinais
p6 = 0.0745 #Tamanho dos im?veis
p7 = 0.0662 #Im?veis certificados
p8 = 0.0454 #Declividade
p9 = 0.0436 #Rios
p10 = 0.0313 #Assentamento
p11 = 0.0250 #Embargo
p12 = 0.0151 #Floresta
p13 = 0.0110 #Unidade de Conserva??o
p14 = 0.0089 #Terra Ind?gena

# criando uma variável na tabela que se chama 'ahp' que será resultado da soma das variáveis em questão ponderada pelos pesos
dados$ahp = (dados$i_aberta * p1)+(dados$i_vegse * p2)+
  (dados$i_br * p3)+(dados$i_au * p4)+(dados$i_est * p5)+
  (dados$i_tamimov * p6)+(dados$i_cert * p7)+(dados$i_decliv * p8)+
  (dados$i_rios5 * p9)+(dados$i_ass_p* p10) +(dados$i_emb* p11) +
  (dados$i_flor13* p12)+(dados$i_uc* p13)+(dados$i_ti* p14)


# Importando a tabela com as simulações nos pesos -------------------------

simu <- read.csv2("simulado50.csv" )
View(simu)

# Aplicando um loop para pegar em uma simulação por vez os pesos determinados 
# para cada variável e aplicar da mesma forma como foi feito para gerar a 
# variável ahp a cima 
s = 1 # contador do loop
while (s <= length(simu$RC)){ # loop do tipo while
  # na tabela 'simu', pegar o peso na coluna nomeada depois do '$' na linha '[s]'
  # obs.: os nomes estão estranhos pq foi assim q ele reconheceu no meu pc
  p1 = simu$?.rea.Aberta[s] # Area Aberta
  p2 = simu$Vegeta?.?.o.Secund?.ria[s] # Vegeta??o Secund?ria
  p3 = simu$BR.163[s] #BR 163
  p4 = simu$Proximidade.a.?.rea.urbana[s] #Proximidade a ?rea urbana
  p5 = simu$Estradas.vicinais[s] #Estradas vicinais
  p6 = simu$Tamanho.dos.imóveis[s] #Tamanho dos im?veis
  p7 = simu$Imóveis.certificados[s] #Im?veis certificados
  p8 = simu$Declividade[s] #Declividade
  p9 = simu$Rios[s] #Rios
  p10 = simu$Assentamento[s] #Assentamento
  p11 = simu$Embargo[s] #Embargo
  p12 = simu$Floresta[s] #Floresta
  p13 = simu$Unidade.de.Conserva?.?.o[s] #Unidade de Conserva??o
  p14 = simu$Terra.Ind?.gena[s] #Terra Ind?gena
  # depois de pegar os pesos, criar uma nova variável na tabela de atributo que é a soma das
  # variáveis em estudo ponderada pelos pesos da simulação
  dados.ahp@data[,ncol(dados.ahp)+1] <- (dados.ahp$i_aberta * p1)+(dados.ahp$i_vegse * p2)+
    (dados.ahp$i_br * p3)+(dados.ahp$i_au * p4)+(dados.ahp$i_est * p5)+
    (dados.ahp$i_tamimov * p6)+(dados.ahp$i_cert * p7)+(dados.ahp$i_decliv * p8)+
    (dados.ahp$i_rios5 * p9)+(dados.ahp$i_ass_p* p10) +(dados.ahp$i_emb* p11) +
    (dados.ahp$i_flor13* p12)+(dados.ahp$i_uc* p13)+(dados.ahp$i_ti* p14)
  # aumentar o contador e voltar para o início do loop.
  s = s+1
  # quando o contador for maior do que o número de simulações, ele terminou de fazer todas 
  # as simulações e o programa sai do loop
}

# checar a tabela
str(dados.ahp@data[,c(80:954)])

# Calcular a Distância InterQuartil (DIQ) para checar a sensibilidade da AHP para cada célula.
# A DIQ é a diferença entre o q1 e o q3 dos resultados de todas as simulações.
# Os resultados das simulações estão nas colunas e cada linha representa uma célula.
# Por isso, uso um loop para calcular o q1 e q3 de cada célular
for(i in 1:length(dados.ahp$id)){ # loop do tipo 'for'
  # entrar na linha '[i]' da tabela, criar variável $'q1', calcular o q1 (0,25) para os valores 
  # da coluna 80 até 954 que são os resultados das simulações
  dados.ahp$q1[i] = quantile(dados.ahp@data[i,c(80:954)], c(0.25))[,1] #SEMPRE CHECAR AS COLUNAS
  # entrar na linha '[i]' da tabela, criar variável $'q3', calcular o q3 (0,75) para os valores 
  # da coluna 80 até 954 que são os resultados das simulações
  dados.ahp$q3[i] = quantile(dados.ahp@data[i,c(80:954)], c(0.75))[,1] #SEMPRE CHECAR AS COLUNAS
}

# Por fim, criar uma variável $'sense' que é a DIQ que consiste no calculo q3 - q1 para cada linha.
# Nesse calculo, o R já entende que é para fazer isso para cada linha, por isso não precisa usar um loop.
dados.ahp$sense = dados.ahp$q3 - dados.ahp$q1

# vizualizar os dados.
View(dados.ahp@data)

# Exportar os dados para um novo shapefile para usar fora do R
writeOGR(dados.ahp, driver = "ESRI Shapefile", dsn = "./grade_celular.shp", layer = "grade_celular_calculada.shp" )