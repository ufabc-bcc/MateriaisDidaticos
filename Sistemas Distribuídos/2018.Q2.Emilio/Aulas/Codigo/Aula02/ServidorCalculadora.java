import java.rmi.registry.Registry;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.UnicastRemoteObject;
public class ServidorCalculadora {
  public static void main(String args[]) {
    try {
      //Crio o objeto servidor
      CalculadoraImpl calc = new CalculadoraImpl();
      //Criamos o stub do objeto que será registrado
      Calculadora stub = (Calculadora)UnicastRemoteObject
          .exportObject(calc, 0);
      //Registra (binds) o stub no registry
      Registry registry = LocateRegistry.getRegistry();
      registry.bind("calculadora", stub);
      System.out.println("Servidor iniciado.");
    } catch (Exception e) {
          System.err.println("Ocorreu um erro no servidor: " +
                             e.toString());
    }
  }
}
