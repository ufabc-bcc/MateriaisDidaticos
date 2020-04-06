'''
ClassBot - Bot para aulas.
Author: Fabricio Olivetti de França
'''

import irc.bot
import requests

class TwitchBot(irc.bot.SingleServerIRCBot):
    def __init__(self, username, client_id, token, channel):
        self.client_id = client_id
        self.token     = token
        self.channel   = '#' + channel
        self.perguntas = []

        # Pega o id do canal para ficar compatível com API v5
        url             = 'https://api.twitch.tv/kraken/users?login=' + channel
        headers         = {'Client-ID': client_id, 'Accept': 'application/vnd.twitchtv.v5+json'}
        resultado       = requests.get(url, headers=headers).json()
        self.channel_id = resultado['users'][0]['_id']

        # Conecta ao IRC
        server = 'irc.chat.twitch.tv'
        port   = 6667
        print('Conectando ao ' + server + ' na porta ' + str(port) + '...')
        irc.bot.SingleServerIRCBot.__init__(self, [(server, port, 'oauth:'+token)], username, username)

    # Preparativos ao conectar no servidor
    def on_welcome(self, conexao, evento):
        print('Entrando no canal ' + self.channel)

        # Requisitando comandos especiais (pode ser inútil)
        conexao.cap('REQ', ':twitch.tv/membership')
        conexao.cap('REQ', ':twitch.tv/tags')
        conexao.cap('REQ', ':twitch.tv/commands')

        # Entrando no canal
        conexao.join(self.channel)

        # Enviando primeira mensagem
        conexao.privmsg(self.channel, "Ola! Eu sou o ClassBot! Pergunte com o comando '!pergunta'")

    def on_pubmsg(self, conexao, evento):

        # Se a mensagem começa com !, tente executar um comando
        if evento.arguments[0][0] == '!':
            partes  = evento.arguments[0].split(' ', 1)
            comando = partes[0][1:]
            if len(partes) > 1:
                pergunta = partes[1]
            else:
                pergunta = ""

            self.do_command(evento, conexao, comando, pergunta)

    def do_command(self, evento, conexao, comando, pergunta):
        allowed  = [""] # preencha com o nick dos professores
        url      = 'https://api.twitch.tv/kraken/channels/' + self.channel_id
        headers  = {'Client-ID': self.client_id, 'Accept': 'application/vnd.twitchtv.v5+json'}
        requests.get(url, headers=headers).json()

        # Procura pelo nick de quem perguntou
        for tag in evento.tags:
            if tag['key'] == 'display-name':
                nick = tag['value']
                print(nick)

        if comando == "pergunta":
            self.perguntas.append(pergunta)
            print("Pergunta adicionada " + ' '.join(self.perguntas))
        elif comando == "proxima" and nick in allowed:
            print(nick)
            if self.perguntas:
                conexao.privmsg(self.channel, self.perguntas[0])
                self.perguntas.pop(0)
                print("Pergunta lida!")
            else:
                conexao.privmsg(self.channel, "Não tem perguntas!")
        elif comando == "lista":
            for i, pergunta in enumerate(self.perguntas):
                conexao.privmsg(self.channel, f"Q{i+1} - {pergunta}")
            print("Perguntas listadas!")

def main():    
    username  = "ClassBot"
    client_id = ""
    token     = ""
    channel   = ""

    bot = TwitchBot(username, client_id, token, channel)
    bot.start()

if __name__ == "__main__":
    main()
