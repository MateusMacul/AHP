---
title: "README"
author: "Mateus de Souza Macul"
date: "23/07/2020"
output: html_document
---
##  AHP (Analytic Hierarchy Process)

> _Processo Analítico Hierárquico_

___


Projeto criado para compartilhar os métodos utilizados em parte das análises na minha dissertaçao de mestrado no INPE, cujo título é ["ÍNDICE DE VALORIZAÇÃO DA TERRA E
DESMATAMENTO EM UMA REGIÃO DE FRONTEIRA AGROPECUÁRIA NA AMAZÔNIA: REGIÃO DE NOVO PROGRESSO, PARÁ"](http://mtc-m21c.sid.inpe.br/rep/sid.inpe.br/mtc-m21c/2019/08.16.19.10?metadatarepository=sid.inpe.br/mtc-m21c/2019/08.16.19.10.47&ibiurl.backgroundlanguage=pt&ibiurl.requiredsite=mtc-m21c.sid.inpe.br+806&requiredmirror=urlib.net/www/2017/11.22.19.04.03&searchsite=bibdigital.sid.inpe.br:80&searchmirror=sid.inpe.br/bibdigital@80/2006/11.11.23.17&choice=briefTitleAuthorMisc). Nesse trabalho, utilizei o AHP para gerar pesos em uma soma ponderada de variáveis. Essa soma é de variáveis que são importantes para derterminar a valorização da terra. O AHP foi necessário, pois a importancia das variáveis pôde ser determinada com base no conhecimento empirico.

### aquivos
1. _AHP 6.0.xlsx_: Planilha de criação da AHP
2. _AHP 6.0 simulações.xlsx_: Arquivo com planilha de criação da AHP e simulações 
3. _ahp.r_: Script com o calculo da soma ponderada, simulações e análise de sensibilidade (ditancia inter quartil)
4. _grade_celular_: Shapefile usado como exemplo no script (arquivo .dbf muito grande para colocar aqui. Posso passar o shape completo a parte)
