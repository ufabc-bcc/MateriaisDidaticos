package sd;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;

public class Contato {

    private int id;
    private String nome;
    private List<Telefone> telefones = new ArrayList<Telefone>();
    private List<Email> emails = new ArrayList<Email>();

    public int getId() {
        return id;
    }

    public String getNome() {
        return nome;
    }

    public List<Telefone> getTelefones() {
        return telefones;
    }

    public List<Email> getEmails() {
        return emails;
    }

    public void setId(int id) {
        this.id = id ;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public void setTelefones(List<Telefone> telefones) {
        this.telefones = telefones;
    }

    public void setEmails(List<Email> emails) {
        this.emails = emails;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj instanceof Contato) &&
            ((Contato)obj).getId() == this.getId();
    }

    @Override
    public int hashCode() {
        return getId();
    }

	@Override
	public String toString() {
		StringBuilder contact = new StringBuilder();
		contact.append("{ ");
		contact.append(getId());
		contact.append(", ");
		contact.append(getNome());
		contact.append(", ");
		contact.append(Arrays.toString(getEmails().toArray()));
		contact.append(", ");
		contact.append(Arrays.toString(getTelefones().toArray()));
		contact.append(" }");

		return contact.toString();
	}


}
