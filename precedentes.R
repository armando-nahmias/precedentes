#!/usr/bin/Rscript

setwd('~/repositorios/precedentes')

cat(epoxy::epoxy('\n\n\n\nInício da execução da consulta: {Sys.time()}.\n\n\n'))

informacoes <- list("1.atualizar.arquivos.STJ.R", "2.preparar.tabela.temas.R", "3.enviar.tabela.html.R")

arquivos.fonte <- file.path('func', informacoes)

lapply(arquivos.fonte, source)


atualizar.arquivos.STJ()

tabela.temas <- preparar.tabela.temas()

enviar.tabela.html(tabela.temas, teste = T)

# # Testes
# system(paste('touch -t 202301010000.00', 'dados/temas.csv'))
# df <- read.csv2('dados/temas.csv')
# df <- head(df, -1)
# write.csv2(df, 'dados/temas.csv')
# 
# 
# # Rastrear erro
# debug(atualizar.arquivos.STJ)

cat(epoxy::epoxy('\n\n\n\nTérmino da execução da consulta: {Sys.time()}.\n\n\n'))

