import java.net.*;
import java.io.*;

public class ClienteREST {
  public static void main(String[] args) throws Exception {
    URL url = new URL("http://localhost:8080/contatos");
    HttpURLConnection conn = (HttpURLConnection) url.openConnection();
    conn.setRequestMethod("GET");
    conn.setRequestProperty("Accept", "application/json");
    if (conn.getResponseCode() != 200) {
      throw new RuntimeException("Failed : HTTP error code : "
        + conn.getResponseCode());
    }
    BufferedReader br = new BufferedReader(new InputStreamReader(
      (conn.getInputStream())));
    String output;
    System.out.println("Output from Server .... \n");
    while ((output = br.readLine()) != null) {
      System.out.println(output);
    }
    conn.disconnect();
  }
}
