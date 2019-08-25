package org.hswebframework.ezorm.rdb.executor.jdbc;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.jdbc.JdbcSyncSqlExecutor;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Collections;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequest.*;
import static org.hswebframework.ezorm.rdb.executor.SqlRequests.prepare;
import static org.hswebframework.ezorm.rdb.executor.SqlRequests.template;
import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;

public class JdbcSyncSqlExecutorTest {


    private SyncSqlExecutor executor;

    @SneakyThrows
    @Before
    public void init() {
        Class.forName("org.h2.Driver");
        Connection connection = DriverManager.getConnection("jdbc:h2:mem:hsweb", "sa", "");
        executor = new JdbcSyncSqlExecutor() {

            @Override
            public Connection getConnection(SqlRequest sqlRequest) {
                return connection;
            }

            @Override
            public void releaseConnection(Connection connection, SqlRequest sqlRequest) {

            }
        };
    }

    @SneakyThrows
    @Test
    public void testSelectSingle() {
        Map<String, Object> data = executor.select(prepare("select 1 as data"), singleMap());
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

    @SneakyThrows
    @Test
    public void testSelectPrepare() {
        Map<String, Object> data = executor.select(prepare("select 1 as data where 1 = ?", 1), singleMap());
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

    @SneakyThrows
    @Test
    public void testSelectTemplate() {
        Map<String, Object> data = executor.select(template("select 1 as data where 1 = #{id}", Collections.singletonMap("id", 1)), singleMap());
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

}