package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.Containers;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.wait.strategy.Wait;

import java.sql.Connection;
import java.sql.DriverManager;

public class MSSQLConnectionProvider implements ConnectionProvider {

    static int port;

    static {
        try {
            Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        try {
            GenericContainer<?> container = Containers.newMSSQL();

            container.start();
            port = container.getMappedPort(1433);
        }catch (Throwable e){
            e.printStackTrace();
        }
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("mssql.username", "sa");
        String password = System.getProperty("mssql.password", "ezorm@PasswOrd");
        String url = System.getProperty("mssql.url", "127.0.0.1:" + port);
//        String db = System.getProperty("mysql.db", "dbo");
        return DriverManager.getConnection("jdbc:sqlserver://" + url, username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
