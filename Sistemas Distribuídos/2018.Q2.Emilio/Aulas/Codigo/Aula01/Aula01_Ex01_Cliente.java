import java.io.*;
import java.net.*;

public class Aula01_Ex01_Cliente {
    public static void main(String[] args) throws IOException {

        if (args.length != 2) {
            System.err.println(
                "Forma de uso: java Aula01_Ex01_Cliente <end. servidor> <porta>");
            System.exit(1);
        }

        String hostName = args[0];
        int portNumber = Integer.parseInt(args[1]);

        try (
            Socket socket = new Socket(hostName, portNumber);
            PrintWriter socketOut =
                new PrintWriter(socket.getOutputStream(), true);
            BufferedReader socketIn =
                new BufferedReader(
                    new InputStreamReader(socket.getInputStream()));
            BufferedReader stdIn =
                new BufferedReader(
                    new InputStreamReader(System.in))
        ) {
            while (true) {
                if (stdIn.ready()) {
                    String userInput = stdIn.readLine();
                    socketOut.println(userInput);
                    if ("SAIR".equals(userInput))
                        break;
                }
                if (socketIn.ready()) {
                    String msg = socketIn.readLine();
                    System.out.println(">" + msg);
                    if ("SAIR".equals(msg))
                        break;
                }
            }
        } catch (UnknownHostException e) {
            System.err.println("Endereço desconhecido " + hostName);
            System.exit(1);
        } catch (IOException e) {
            System.err.println("Erro conectanto em: " +
                hostName);
            System.exit(1);
        }
    }
}
