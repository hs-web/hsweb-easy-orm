package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.Containers;
import org.junit.Assert;
import org.postgresql.Driver;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;

import java.sql.Connection;
import java.sql.DriverManager;

public class PostgresqlConnectionProvider implements ConnectionProvider {

    static int port;

    static {
        Assert.assertTrue(Driver.isRegistered());
        GenericContainer<?> container = Containers.newPostgresql("11");

        container.waitingFor(Wait.forListeningPort());
        container.start();
        port = container.getMappedPort(5432);
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("postgres.username", "postgres");
        String password = System.getProperty("postgres.password", "admin");
        String url = System.getProperty("postgres.url", "127.0.0.1:" + port);
        String db = System.getProperty("postgres.db", "ezorm");
        return DriverManager.getConnection("jdbc:postgresql://" + url + "/" + db, username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
