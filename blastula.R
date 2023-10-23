mensagem <- blastula::compose_email(
  body = 'Oie. Este é um teste.'
)



blastula::create_smtp_creds_key(id = 'api',
                                user = 'informartizar@gmail.com',
                                host = "smtp.gmail.com",
                                port = 465,
                                use_ssl = T,
                                overwrite = T)


blastula::view_credential_keys()

blastula::smtp_send(email = novo.email,to = 'armando@informartizar.com', from = 'informartizar@gmail.com', subject = 'Teste', credentials = credenciais)

novo.email <- blastula::render_email(teste)
teste <- blastula::md('Teste.qmd')


blastula::blastula_email()

htmltools::html_print(novo.email)
