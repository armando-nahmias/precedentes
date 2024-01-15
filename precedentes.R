
informacoes <- list("1.atualizar.arquivos.STJ.R", "2.preparar.tabela.temas.R", "3.enviar.tabela.html.R")

arquivos.fonte <- file.path('func', informacoes)

lapply(arquivos.fonte, source)


atualizar.arquivos.STJ()

tabela.temas <- preparar.tabela.temas()

enviar.tabela.html(tabela.temas, teste = T)

# Testes
system(paste('touch -t 202301010000.00', 'dados/dicionario-temas.csv'))
df <- read.csv2('dados/dicionario-temas.csv')
df <- head(df, -1)
write.csv2(df, 'dados/dicionario-processos.csv')


# Rastrear erro
debug(enviar.tabela.html)
