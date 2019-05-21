package sd;

public class Email {

    private String tipo;
    private String endereco;

    public Email() {}

    public Email(String tipo, String endereco) {
        this.tipo = tipo;
        this.endereco = endereco;
    }

    public String getTipo() {
        return tipo;
    }

    public void setTipo(String tipo) {
        this.tipo = tipo;
    }

    public String getEndereco() {
        return endereco;
    }

    public void setEndereco(String endereco) {
        this.endereco = endereco;
    }

    public String toString() {
        StringBuilder email = new StringBuilder();
        email.append("{ ");
        email.append(getTipo() + ":" + getEndereco());
        email.append(" }");

        return email.toString();
    }
}
