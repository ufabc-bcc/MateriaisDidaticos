import java.net.*;
import java.io.*;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class Aula01_Ex02_Servidor {

    static Set<ConexaoCliente> clientes = ConcurrentHashMap.newKeySet();

    static class ConexaoCliente implements Runnable {

        private final Socket socket;
        private final PrintWriter clientOut;
        private final BufferedReader clientIn;

        public ConexaoCliente (Socket clientSocket) throws IOException {
            socket = clientSocket;
            clientOut =
                new PrintWriter(clientSocket.getOutputStream(), true);
            clientIn = new BufferedReader(
                new InputStreamReader(clientSocket.getInputStream()));
            clientes.add(this);
        }

        public void enviaMensagem(String msg) {
            clientOut.println(msg);
        }

        public void run () {
            try{
                while (true) {
                    String msg = clientIn.readLine();
                    if (msg == null || msg.endsWith("SAIR"))
                        break;
                    for (ConexaoCliente conexao : clientes) {
                        if (conexao == this) continue;
                        conexao.enviaMensagem(msg);
                    }
                    System.out.println(msg);

                }
            } catch (IOException e) {
                System.out.println("> Ocorreu um erro com uma conexão:"
                    + e.getMessage());
            } finally{
                clientes.remove(this);
                System.out.println("> Cliente desconectado. Conexões ativas: "
                    + clientes.size());
                try{clientIn.close();
                clientOut.close();
                socket.close();} catch (IOException e) {
                    System.out.println("> Ocorreu um erro tentando fechar a conexão:"
                        + e.getMessage());
                }
            }
        }
    }

    public static void main(String[] args) throws IOException {

        if (args.length != 1) {
            System.err.println("Forma de uso: java Aula01_Ex02_Servidor <porta>");
            System.exit(1);
        }

        int portNumber = Integer.parseInt(args[0]);

        ServerSocket serverSocket =
                new ServerSocket(Integer.parseInt(args[0]));
        while (true) {
            System.out.println("> Esperando usuários. Conexões ativas: " + clientes.size());
            Socket clientSocket = serverSocket.accept();
            ConexaoCliente cliente = new ConexaoCliente(clientSocket);
            new Thread(cliente).start();
            System.out.println("> Recebida uma conexão.");
        }
    }

}
