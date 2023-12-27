# googledrive::drive_auth()

# googledrive::drive_mkdir('precedentes')

df <- googledrive::drive_get('precedentes/tabela.temas.html')

googledrive::drive_upload(media = 'saida/tabela-20231219-002853.html', path = 'precedentes/tabela.temas.html')

googledrive::drive_reveal(df$id, 'published')
tt <- googledrive::drive_reveal(df$id, 'permissions')


googledrive::drive_share_anyone(df$id)

