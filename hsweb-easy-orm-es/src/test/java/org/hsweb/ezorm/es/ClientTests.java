package org.hsweb.ezorm.es;

import org.apache.http.HttpEntity;
import org.apache.http.HttpHost;
import org.apache.http.entity.StringEntity;
import org.apache.http.util.EntityUtils;
import org.elasticsearch.client.Response;
import org.elasticsearch.client.RestClient;

import java.io.IOException;
import java.util.Collections;


/**
 * @author zhouhao
 */
public class ClientTests {
    public static void main(String[] args) throws IOException {
        RestClient client = RestClient.builder(
                new HttpHost("localhost", 9200, "http")).build();
        String json ="{\n" +
                "  \"query\": {\n" +
                "    \"bool\": {\n" +
                "      \"must\":     { \"match\": { \"title\": \"我\" }}\n" +
                "      \n" +
                "    }\n" +
                "  }\n" +
                "}";
        HttpEntity entity = new StringEntity(json);

        Response response = client.performRequest("GET", "/hsweb/test/_search", Collections.singletonMap("pretty", "true"), entity);

        System.out.println(EntityUtils.toString(response.getEntity()));

//
//        IndexResponse response = client.prepareIndex("hsweb", "test")
//                .setSource(jsonBuilder()
//                        .startObject()
//                        .field("test", "aaaa")
//                        .endObject()).get();
//        System.out.println(response);
        client.close();
    }
}
