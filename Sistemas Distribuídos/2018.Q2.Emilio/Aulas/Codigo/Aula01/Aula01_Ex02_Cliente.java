import java.io.*;
import java.net.*;

public class Aula01_Ex02_Cliente {

    static class ConexaoServidor implements Runnable {

        private final Socket socket;
        private final PrintWriter socketOut;
        private final BufferedReader socketIn;
        private final String nick;

        public ConexaoServidor (Socket socket, String nick) throws IOException {
            this.socket = socket;
            this.nick = nick;
            socketOut =
                new PrintWriter(socket.getOutputStream(), true);
            socketIn = new BufferedReader(
                new InputStreamReader(socket.getInputStream()));
        }

        public void enviaMensagem(String msg) {
            socketOut.println(nick + "> " +  msg);
        }

        public void run () {
            try{
                while (true) {
                    String msg = socketIn.readLine();
                    if (msg == null) {
                        System.out.println("Conexão perdida.");
                        System.exit(1);
                    }
                    System.out.println(msg);
                }
            } catch (Exception e) {
                System.out.println(e.getMessage());
            } finally {
                try {
                    socketIn.close();
                    socketOut.close();
                    socket.close();
                } catch (Exception e) {
                    //já não ha mais o que se fazer
                }
            }
        }
    }


    public static void main(String[] args) throws IOException {

        if (args.length != 3) {
            System.err.println(
                "Forma de uso: java Aula01_Ex02_Cliente <end. servidor> <porta> <nick>");
            System.exit(1);
        }

        String hostName = args[0];
        int portNumber = Integer.parseInt(args[1]);
        String nick = args[2];

        try (
             Socket socket = new Socket(hostName, portNumber);
             BufferedReader stdIn =
             new BufferedReader(new InputStreamReader(System.in))
             ) {
            ConexaoServidor conexaoServidor = new ConexaoServidor(socket, nick);
            Thread th = new Thread (conexaoServidor);
            th.setDaemon(true);
            th.start();
            String userInput;
            while ((userInput = stdIn.readLine()) != null) {
                if ("SAIR".equals(userInput)) break;
                conexaoServidor.enviaMensagem(userInput);
            }
        } catch (UnknownHostException e) {
            System.err.println("Endereço desconhecido " + hostName);
            System.exit(1);
        } catch (IOException e) {
            System.err.println("Erro conectanto em: " + hostName);
            System.exit(1);
        }
    }
}
