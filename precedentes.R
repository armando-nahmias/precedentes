source('func/baixar_arquivos_STJ.R')

baixar.arquivo.STJ()

df <- readRDS('dados/dados-completos.rds')
df.temas <- df$temas.csv

tabela.temas <- df.temas[,c('numeroPrecedente', 'questaoSubmetidaAJulgamento', 'teseFirmada', 'Assuntos', 'dataPublicacaoAcordao', 'teseFirmada')]
