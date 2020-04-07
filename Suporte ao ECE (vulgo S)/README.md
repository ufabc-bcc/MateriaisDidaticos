# Ferramentas de suporte ao ECE

1. classbot_exemplo.py: Bot para organizar as perguntas no chat do Twitch:
   * Instruções de uso:
      - Instale a biblioteca IRC com `pip install irc`
      - Crie uma conta em https://www.twitch.tv/ para fazer Stream
      - Crie uma conta em https://www.twitch.tv/ para o bot
      - Registre um aplicativo em https://dev.twitch.tv/console/apps e obtenha um ID de cliente (pode deixar a url como localhost)
      - Com o usuário do bot, entre em https://twitchapps.com/tmi/ e obtenha uma senha OAUTH (token)
      - Alterar o arquivo config_exemplo.yaml com o nome do bot, o id de cliente, o OAUTH e o nome do seu usário de stream
      - Acrescente o campo admins no arquivo yaml com os nomes de usuários que podem recuperar perguntas para serem respondidas
      - Os comandos são `!pergunta texto da pergunta` para fazer uma pergunta, `!lista` para listar as perguntas, `!upvote QN` para votar na pergunta N, `!proxima` para recuperar a próxima pergunta mais votada (apenas admin) e `!limpar` para apagar todas as perguntas (apenas admin).
