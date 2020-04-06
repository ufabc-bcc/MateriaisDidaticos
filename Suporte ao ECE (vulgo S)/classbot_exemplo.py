'''
ClassBot - Bot para aulas.
Author: Fabricio Olivetti de França
'''

import irc.bot
import requests
import yaml
import sys
import os


class TwitchBot(irc.bot.SingleServerIRCBot):
    def __init__(self, username, client_id, token, channel, allowed):
        self.client_id = client_id
        self.token     = token
        self.channel   = '#' + channel
        self.perguntas = []
        self.lidas = []
        self.upvotes = []
        self.proxima = None
        self.allowed = allowed

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
        url      = 'https://api.twitch.tv/kraken/channels/' + self.channel_id
        headers  = {'Client-ID': self.client_id, 'Accept': 'application/vnd.twitchtv.v5+json'}
        requests.get(url, headers=headers).json()

        # Procura pelo nick de quem perguntou
        for tag in evento.tags:
            if tag['key'] == 'display-name':
                nick = tag['value']
                print(nick)
                break

        if comando == "pergunta":
            self.perguntas.append(pergunta)
            self.upvotes.append([nick])
            self.lidas.append(False)
            print("Pergunta adicionada " + ' '.join(self.perguntas))

        elif comando == "upvote":
            print(pergunta)
            try:
                idx_pergunta = int(pergunta[1:])
            except ValueError:
                return

            if idx_pergunta >= 0 and idx_pergunta < len(self.perguntas):
                if not self.lidas[idx_pergunta] and not nick in self.upvotes[idx_pergunta]:
                    self.upvotes[idx_pergunta].append(nick)
                    if self.proxima is None or len(self.upvotes[idx_pergunta]) > len(self.upvotes[self.proxima]):
                        self.proxima = idx_pergunta

        elif comando == "proxima" and nick in self.allowed:
            if self.perguntas:
                if self.proxima:
                    conexao.privmsg(self.channel, self.perguntas[self.proxima])
                    self.lidas[self.proxima] = True
                    max_votos = -1
                    self.proxima = None
                    for i in range(len(self.perguntas)):
                        if not self.lidas[i] and len(self.upvotes[i]) > max_votos:
                            max_votos = len(self.upvotes[i])
                            self.proxima = i
                else:
                    for i in range(len(self.perguntas)):
                        if not self.lidas[i]:
                            conexao.privmsg(self.channel, self.perguntas[i])
                            self.lidas[i] = True
                            break

                print("Pergunta lida!")
            else:
                conexao.privmsg(self.channel, "Não tem perguntas!")

        elif comando == "lista":
            if len(self.perguntas) == 0:
                conexao.privmsg(self.channel, "Não temos perguntas ainda")
            else:
                for i, pergunta in enumerate(self.perguntas):
                    resp = "[respondida]" if self.lidas[i]  else ""
                    nupv = len(self.upvotes[i])
                    autor = self.upvotes[i][0]
                    conexao.privmsg(self.channel, f"Q{i} - {pergunta} feita por {autor} - {nupv} votos {resp}")
                print("Perguntas listadas!")

        elif comando == "limpar" and nick in self.allowed:
            self.perguntas = []
            self.lidas = []
            self.proxima = None
            self.upvotes = []
            conexao.privmsg(self.channel, f"Reiniciando perguntas e contagens, pedido por {nick}")

        else:
            conexao.privmsg(self.channel, f"Comando {comando} não reconhecido ou não permitido")

def main():
    args = sys.argv[1:]
    if len(args) == 0:
        config = "config.yaml"
    elif len(args) == 1:
        config = args[0]
    else:
        print(f"Modo de usar: {sys.argv[0]} config.yaml")
        return

    if not os.path.isfile(config):
        print(f"O arquivo {config} não existe")
        return

    with open(config, "r") as f:
        cfg = yaml.load(f, Loader=yaml.FullLoader)
        username  = cfg['nome']
        client_id = cfg['client_id']
        token     = cfg['token']
        channel   = cfg['canal']
        allowed   = cfg['admins']

        bot = TwitchBot(username, client_id, token, channel, allowed)
        bot.start()

if __name__ == "__main__":
    main()
