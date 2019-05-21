package sd;

import java.util.ArrayList;
import java.util.List;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.GenericEntity;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

@Path("contatos")
public class ServicoContatos {

    private static Map<Integer, Contato> contatos = new HashMap<Integer, Contato>();
    private static int proximoId = 1;

    private void criaContato(String nome, String emailTipo, String email, String telTipo, String tel) {
        int id = proximoId++;
        Contato contato = new Contato();
        contato.setId(id);
        contato.setNome(nome);
        contato.getEmails().add(new Email(emailTipo, email));
        contato.getTelefones().add(new Telefone(telTipo, tel));
        contatos.put(id, contato);
    }

    public ServicoContatos() {

        if (contatos.isEmpty()) {

            criaContato("E. Francesquini",
                        "Profissional", "e.francesquini@ufbac.edu.br",
                        "Comercial", "9876 5432");
            criaContato("F. Ferreira ",
                        "Profissional", "fernando.teubl@ufabc.edu.br",
                        "Residencial", "1234 5678");
        }
    }

    @GET
    @Produces(MediaType.TEXT_PLAIN)
    public String listPlain() {
        System.out.println("listPlain");
        StringBuffer ret = new StringBuffer();
        for (Contato contato :contatos.values()) {
            ret.append(contato);
            ret.append("\n");
        }
        return ret.toString();
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public List<Contato> listJson() {
        System.out.println("listJson");
        return new ArrayList(contatos.values());
    }

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("/{id}")
    public Contato getContato(@PathParam("id") int id) {
        System.out.println("getContato");
        Contato ret = contatos.get(id);
        if (ret == null)
            throw new WebApplicationException(404);
        return ret;
    }

    @GET
    @Path("/busca/{nome}")
    @Produces(MediaType.APPLICATION_JSON)
    public List<Contato> findByName(@PathParam("nome") String nome) {
        System.out.println("findByName");
        String lcNome = nome.toLowerCase();
        List<Contato> ret = new ArrayList<Contato>();
        for (Contato contato : contatos.values()) {
            if (contato.getNome() != null
                && contato.getNome().toLowerCase().contains(lcNome)) {
                ret.add(contato);
            }
        }
        return ret;
    }

    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Contato add(Contato contato) {
        System.out.println("add");
        if (contato.getNome() == null || contato.getNome().trim().equals("")) {
            throw new WebApplicationException(Response
                                              .status(Response.Status.BAD_REQUEST)
                                              .entity("Nome é obrigatorio").build());
        }
        contato.setId(proximoId++);
        contatos.put(contato.getId(), contato);
        return contato;
    }

    @PUT
    @Path("/{id}")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response update(@PathParam("id") int id, Contato contato) {
        System.out.println("update");
        if (id != contato.getId()) {
            throw new WebApplicationException(Response
                                              .status(Response.Status.BAD_REQUEST)
                                              .entity("O id do contato deve ser igual ao id utilizado").build());
        }
        contatos.put(id, contato);
        return Response.ok().build();
    }

    @DELETE
    public Response delete(@QueryParam("id") int id) {
        System.out.println("delete");
        Contato cont = contatos.get(id);
        if (cont == null) {
            System.out.println("Não achou");
            throw new WebApplicationException(404);
        }

        contatos.remove(id);
        return Response.ok().build();
    }

}
