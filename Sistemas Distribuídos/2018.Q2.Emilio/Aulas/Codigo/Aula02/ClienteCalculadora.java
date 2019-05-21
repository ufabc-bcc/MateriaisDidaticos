import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;

public class ClienteCalculadora {
  public static void main(String[] args) {
    try {

        // Localiza o registry. É possível usar endereço/IP porta
       Registry registry = LocateRegistry.getRegistry(null);
       // Consulta o registry e obtém o stub para o objeto remoto
       Calculadora calc = (Calculadora) registry
           .lookup("calculadora");
       // A partir deste momento, cahamadas à Caluladora podem ser
       // feitas como qualquer chamada a métodos

       Numero num1 = new NumeroImpl(3);
       Numero num2 = new NumeroImpl(4);
       //Aqui são feitas diversas chamadas remotas
       Numero soma = calc.soma(num1, num2);
       Numero sub = calc.subtrai(num1, num2);
       Numero mult = calc.multiplica(num1, num2);
       Numero div = calc.divide(num1, num2);
       System.out.println("Resultados obtidos do servidor:" +
                          "\n\t+:" + soma.getValor() +
                          "\n\t-:" + sub.getValor()  +
                          "\n\t*:" + mult.getValor() +
                          "\n\t/:" + div.getValor());

       try {
           calc.divide(new NumeroImpl(1), new NumeroImpl(0));
       } catch (DivisaoPorZeroException e) {
           System.out.println(
             "Tentou dividir por zero! Esta é uma exceção remota.");
       }

    } catch (Exception e) {
       System.err.println("Ocorreu um erro no cliente: " +
                          e.toString());
    }
  }
}
