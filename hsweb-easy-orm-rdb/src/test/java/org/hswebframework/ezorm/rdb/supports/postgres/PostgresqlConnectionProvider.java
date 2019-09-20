package org.hswebframework.ezorm.rdb.supports.postgres;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.junit.Assert;
import org.postgresql.Driver;

import java.sql.Connection;
import java.sql.DriverManager;

public class PostgresqlConnectionProvider implements ConnectionProvider {

    static {
        Assert.assertTrue(Driver.isRegistered());
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("postgres.username", "postgres");
        String password = System.getProperty("postgres.password", "admin");
        String url = System.getProperty("postgres.url", "127.0.0.1:15432");
        String db = System.getProperty("postgres.db", "ezorm");
        return DriverManager.getConnection("jdbc:postgresql://" + url + "/" + db, username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
