package org.hswebframework.ezorm.rdb.supports.mysql;

import com.mysql.jdbc.Driver;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.ConnectionProvider;
import org.hswebframework.ezorm.rdb.Containers;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.testcontainers.containers.GenericContainer;

import java.sql.Connection;
import java.sql.DriverManager;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapList;

public class Mysql57ConnectionProvider implements ConnectionProvider {

    static int port;
    static {
        GenericContainer<?> container = Containers.newMysql("5.7");

        container.start();

        port = container.getMappedPort(3306);

        Driver.getPlatform();
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("mysql.username", "root");
        String password = System.getProperty("mysql.password", "root");
        String url = System.getProperty("mysql.url", "127.0.0.1:"+port);
        String db = System.getProperty("mysql.db", "ezorm");
        return DriverManager.getConnection("jdbc:mysql://" + url + "/" + db + "?useSSL=false", username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }

    public static void main(String[] args) {
        SyncSqlExecutor executor  =new TestSyncSqlExecutor(new Mysql57ConnectionProvider());;
        System.out.println(executor.select(SqlRequests.of("show index from ezorm.table_name"),mapList()));

    }
}
