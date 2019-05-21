package sd;

public class Telefone {

    private String tipo;
    private String numero;

    public Telefone() {}

    public Telefone(String tipo, String numero) {
        this.tipo = tipo;
        this.numero = numero;
    }

    public String getTipo() {
        return tipo;
    }

    public void setTipo(String tipo) {
        this.tipo = tipo;
    }

    public String getNumero() {
        return numero;
    }

    public void setNumero(String numero) {
        this.numero = numero;
    }

    @Override
    public String toString() {
        StringBuilder phone = new StringBuilder();
        phone.append("{ ");
        phone.append(getTipo() + ":" + getNumero());
        phone.append(" }");

        return phone.toString();
    }



}
