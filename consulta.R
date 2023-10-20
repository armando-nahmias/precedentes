# Instale os pacotes se ainda não estiverem instalados
install.packages("RSelenium")
install.packages("rvest")

# Carregue as bibliotecas
library(RSelenium)
library(rvest)

# Inicialize o driver do Selenium (verifique se o Selenium está em execução)
driver <- rsDriver(browser = "chrome")
remDr <- driver$client

# Acesse a página de pesquisa avançada do STF
url <- "https://portal.stf.jus.br/jurisprudenciaRepercussao/pesquisarProcesso.asp"
remDr$navigate(url)

# Obtenha a data de início (hoje - 10 dias)
dataInicio <- Sys.Date() - 10
dataInicio <- format(dataInicio, "%d/%m/%Y")

# Obtenha a data de fim (hoje)
dataFim <- Sys.Date()
dataFim <- format(dataFim, "%d/%m/%Y")

# Preencha as datas nos campos corretos
remDr$executeScript(paste('document.getElementById("dataFinalJulgPV").value = "', dataFim, '";', sep = ""))
remDr$executeScript(paste('document.getElementById("dataInicialJulgPV").value = "', dataInicio, '";', sep = ""))

# Clique no botão de pesquisa
remDr$findElement(using = "xpath", value = '//*[(@id = "botao-pesquisar")]')$clickElement()

# Encerre o driver quando terminar
remDr$close()

# Encerre a instância do Selenium
driver$server$stop()
