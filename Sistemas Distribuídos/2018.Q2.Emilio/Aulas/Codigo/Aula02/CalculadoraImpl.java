public class CalculadoraImpl implements Calculadora {

    public Numero soma (Numero a, Numero b) {
        return new NumeroImpl (a.getValor() + b.getValor());
    };

    public Numero subtrai (Numero a, Numero b) {
        return new NumeroImpl (a.getValor() - b.getValor());
    };

    public Numero multiplica (Numero a, Numero b)  {
        return new NumeroImpl (a.getValor() * b.getValor());
    };

    public Numero divide (Numero a, Numero b)
        throws DivisaoPorZeroException  {
        if (b.getValor() == 0) throw new DivisaoPorZeroException();
        return new NumeroImpl (a.getValor() / b.getValor());
    };

}
