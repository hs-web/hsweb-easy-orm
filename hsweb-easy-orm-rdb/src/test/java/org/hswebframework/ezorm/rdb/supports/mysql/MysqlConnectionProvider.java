package org.hswebframework.ezorm.rdb.supports.mysql;

import com.mysql.jdbc.Driver;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.ConnectionProvider;
import org.hswebframework.ezorm.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;

import java.sql.Connection;
import java.sql.DriverManager;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.mapList;

public class MysqlConnectionProvider implements ConnectionProvider {

    static {
        Driver.getPlatform();
    }

    @SneakyThrows
    public Connection getConnection() {

        String username = System.getProperty("mysql.username", "root");
        String password = System.getProperty("mysql.password", "root");
        String url = System.getProperty("mysql.url", "127.0.0.1:13306");
        String db = System.getProperty("mysql.db", "ezorm");
        return DriverManager.getConnection("jdbc:mysql://" + url + "/" + db + "?useSSL=false", username, password);

    }

    @Override
    @SneakyThrows
    public void releaseConnect(Connection connection) {
        connection.close();
    }

    public static void main(String[] args) {
        SyncSqlExecutor executor  =new TestSyncSqlExecutor(new MysqlConnectionProvider());;
        System.out.println(executor.select(SqlRequests.of("show index from ezorm.table_name"),mapList()));

    }
}
