import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Calculadora extends Remote {
    public Numero soma (Numero a, Numero b)
        throws RemoteException;

    public Numero subtrai (Numero a, Numero b)
        throws RemoteException;

    public Numero multiplica (Numero a, Numero b)
        throws RemoteException;

    public Numero divide (Numero a, Numero b)
        throws RemoteException, DivisaoPorZeroException;
 }
