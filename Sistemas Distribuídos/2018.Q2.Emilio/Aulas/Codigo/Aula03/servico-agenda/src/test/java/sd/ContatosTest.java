package sd;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;

import org.glassfish.grizzly.http.server.HttpServer;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.assertEquals;

public class ContatosTest {

    private HttpServer server;
    private WebTarget target;

    @Before
    public void setUp() throws Exception {
        // start the server
        server = Main.startServer();
        // create the client
        Client c = ClientBuilder.newClient();

        // uncomment the following line if you want to enable
        // support for JSON in the client (you also have to uncomment
        // dependency on jersey-media-json module in pom.xml and Main.startServer())
        // --
        // c.configuration().enable(new org.glassfish.jersey.media.json.JsonJaxbFeature());

        target = c.target(Main.BASE_URI);
    }

    @After
    public void tearDown() throws Exception {
        server.stop();
    }


    @Test
    public void testGetIt() {
        String responseMsg = target.path("contatos").request().get(String.class);
        String expected = "{ 1, E. Francesquini, [{ Profissional:e.francesquini@ufbac.edu.br }], [{ Comercial:9876 5432 }] }\n" +
            "{ 2, F. Ferreira , [{ Profissional:fernando.teubl@ufabc.edu.br }], [{ Residencial:1234 5678 }] }\n";
        assertEquals(expected, responseMsg);
    }
}
