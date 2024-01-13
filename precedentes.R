

arquivos.fonte <- file.path('func', c('baixar_arquivos_STJ.R', 'preparar.tabela.temas.R', 'enviar.tabela.R'))
 
lapply(arquivos.fonte, source)


baixar.info.STJ()

tabela.temas <- preparar.tabela.temas()

enviar.tabela.html(tabela.temas, teste = T)

