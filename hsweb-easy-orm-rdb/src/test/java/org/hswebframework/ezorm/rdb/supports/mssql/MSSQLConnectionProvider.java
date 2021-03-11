package org.hswebframework.ezorm.rdb.supports.mssql;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;

import java.sql.Connection;
import java.sql.DriverManager;

public class MSSQLConnectionProvider implements ConnectionProvider {

    static {
        try {
            Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("mssql.username", "sa");
        String password = System.getProperty("mssql.password", "ezorm@PasswOrd");
        String url = System.getProperty("mssql.url", "127.0.0.1:11433");
//        String db = System.getProperty("mysql.db", "dbo");
        return DriverManager.getConnection("jdbc:sqlserver://" + url, username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
