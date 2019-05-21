import java.net.*;
import java.io.*;
import java.util.*;

public class Aula01_Ex03_Servidor {

    static Set<Cliente> clientes = new HashSet<>();

    static class Cliente {
        InetAddress endereco;
        int porta;

        public Cliente (DatagramPacket pkt) {
            endereco = pkt.getAddress();
            porta = pkt.getPort();
        }

        static Cliente atualizaClientes (DatagramPacket pkt) {
            Cliente ret = new Cliente(pkt);
            clientes.add(ret);
            return ret;
        }

        public boolean equals(Object o) {
             return
                 o != null &&
                 getClass() == o.getClass() &&
                 endereco.equals(((Cliente)o).endereco) &&
                 porta == ((Cliente)o).porta;
        }

        public int hashCode() {
            return Objects.hash(endereco, porta);
        }

        public void enviaMensagem(byte[] buffer, DatagramSocket socket) throws IOException {
            DatagramPacket datagrama = new DatagramPacket (
                buffer, buffer.length,
                endereco, porta);
            socket.send (datagrama);
        }
    }


    public static void main(String[] args) {

        if (args.length != 1) {
            System.err.println("Forma de uso: java Aula01_Ex03_Servidor <porta>");
            System.exit(1);
        }

        int portNumber = Integer.parseInt(args[0]);


        try(
            DatagramSocket serverSocket =
                new DatagramSocket (Integer.parseInt(args[0]));
            ){
            while (true) {
                System.out.println("> Esperando mensagens.");
                DatagramPacket msg = new DatagramPacket (new byte[512], 512);
                serverSocket.receive (msg);
                System.out.println(new String(msg.getData()));
                Cliente cliente = Cliente.atualizaClientes(msg);
                for (Cliente cli: clientes) {
                    if (cli.equals(cliente)) continue;
                    cli.enviaMensagem(msg.getData(), serverSocket);
                }
            }
        } catch (IOException e) {
            System.out.println("Encerrando conexão.");
        }
    }

}
