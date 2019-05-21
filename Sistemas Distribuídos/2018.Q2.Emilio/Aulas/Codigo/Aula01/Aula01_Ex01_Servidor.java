import java.net.*;
import java.io.*;

public class Aula01_Ex01_Servidor {
    public static void main(String[] args) throws IOException {

        if (args.length != 1) {
            System.err.println("Forma de uso: java Aula01_Ex01_Servidor <porta>");
            System.exit(1);
        }

        int portNumber = Integer.parseInt(args[0]);

        try (
            ServerSocket serverSocket =
                new ServerSocket(Integer.parseInt(args[0]));
            Socket clientSocket = serverSocket.accept();
            PrintWriter clientOut =
                new PrintWriter(clientSocket.getOutputStream(), true);
            BufferedReader clientIn = new BufferedReader(
                new InputStreamReader(clientSocket.getInputStream()));
            BufferedReader stdIn =
                new BufferedReader(new InputStreamReader(System.in))
        ) {
            while (true) {
                if (stdIn.ready()) {
                    String userInput = stdIn.readLine();
                    clientOut.println(userInput);
                    if ("SAIR".equals(userInput))
                        break;
                }
                if (clientIn.ready()) {
                    String msg = clientIn.readLine();
                    System.out.println(">" + msg);
                    if ("SAIR".equals(msg))
                        break;
                }
            }
        } catch (IOException e) {
            System.out.println("Ocorreu um erro ao tentar escutar na porta "
                + portNumber);
            System.out.println(e.getMessage());
        }
    }
}
