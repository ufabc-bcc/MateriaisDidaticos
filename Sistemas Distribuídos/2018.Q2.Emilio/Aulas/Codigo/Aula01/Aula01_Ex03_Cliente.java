import java.io.*;
import java.net.*;

public class Aula01_Ex03_Cliente {

    static class ConexaoServidor implements Runnable {

        private final DatagramSocket socket;

        public ConexaoServidor (DatagramSocket socket) {
            this.socket = socket;
        }

        public void run () {
            try{
                while (true) {
                    DatagramPacket resposta =
                        new DatagramPacket (new byte[512], 512);
                    socket.receive (resposta);
                    System.out.println (new String(resposta.getData()));
                }
            } catch (Exception e) {
                System.out.println(e.getMessage());
            } finally {
                try {
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
                "Forma de uso: java Aula01_Ex03_Cliente <end. servidor> <porta> <nick>");
            System.exit(1);
        }

        String hostName = args[0];
        int porta = Integer.parseInt(args[1]);
        String nick = args[2];

        try (
             DatagramSocket socket = new DatagramSocket();
             BufferedReader stdIn = new BufferedReader(
                 new InputStreamReader(System.in))
             ) {
            InetAddress addr = InetAddress.getByName(hostName);
            ConexaoServidor conexaoServidor = new ConexaoServidor(socket);
            Thread th = new Thread (conexaoServidor);
            th.setDaemon(true);
            th.start();

            String userInput;
            while ((userInput = stdIn.readLine()) != null) {
                if ("SAIR".equals(userInput)) break;
                byte[] buffer = (nick + "> " + userInput).getBytes();
                DatagramPacket datagrama = new DatagramPacket (
                    buffer, buffer.length, addr, porta);
                socket.send (datagrama);
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
