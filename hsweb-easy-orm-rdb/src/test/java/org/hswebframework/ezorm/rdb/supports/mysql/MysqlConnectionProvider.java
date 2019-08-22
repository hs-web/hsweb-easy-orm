package org.hswebframework.ezorm.rdb.supports.mysql;

import com.mysql.jdbc.Driver;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.ConnectionProvider;

import java.sql.Connection;
import java.sql.DriverManager;

public class MysqlConnectionProvider implements ConnectionProvider {

    static {
        Driver.getPlatform();
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("mysql.username", "root");
        String password = System.getProperty("mysql.password", "root");
        String url = System.getProperty("mysql.url", "127.0.0.1:3306");
        String db = System.getProperty("mysql.db", "ezorm");
        return DriverManager.getConnection("jdbc:mysql://" + url + "/" + db + "?useSSL=false", username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }
}
