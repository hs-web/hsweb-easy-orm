package org.hswebframework.ezorm.rdb.supports.opengauss;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.Containers;
import org.junit.Assert;
import org.postgresql.Driver;
import org.slf4j.LoggerFactory;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.output.Slf4jLogConsumer;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.DockerImageName;

import java.sql.Connection;
import java.sql.DriverManager;

public class OpengaussConnectionProvider implements ConnectionProvider {

    static int port;

    static void load(){}
    static {
        Assert.assertTrue(Driver.isRegistered());
        GenericContainer<?> container =
            new GenericContainer<>(
                DockerImageName.parse("enmotech/opengauss-lite:5.0.1"))
                .withEnv("TZ", "Asia/Shanghai")
                .withEnv("GS_PASSWORD", "Admin@1234Hs")
                .withExposedPorts(5432)
                .withPrivilegedMode(true)
                .withLogConsumer(new Slf4jLogConsumer(LoggerFactory.getLogger(OpengaussConnectionProvider.class)))
                .waitingFor(Wait.forListeningPort());;

        container.waitingFor(Wait.forListeningPort());
        container.start();
        port = container.getMappedPort(5432);
        try {
            Thread.sleep(5000);
        } catch (InterruptedException ignore) {

        }
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("gauss.username", "gaussdb");
        String password = System.getProperty("gauss.password", "Admin@1234Hs");
        String url = System.getProperty("gauss.url", "127.0.0.1:" + port);
        String db = System.getProperty("gauss.db", "postgres");
        return DriverManager.getConnection("jdbc:postgresql://" + url + "/" + db, username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
