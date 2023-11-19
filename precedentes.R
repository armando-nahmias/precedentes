source('func/baixar_arquivos_STJ.R')

baixar.arquivo.STJ()

source('func/preparar.tabela.temas.R')

df <- preparar.tabela.temas()

source('func/enviar.tabela.R')

enviar.tabela.html(df)
