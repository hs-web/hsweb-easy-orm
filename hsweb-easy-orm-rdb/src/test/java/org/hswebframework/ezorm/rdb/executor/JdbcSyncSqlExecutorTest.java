package org.hswebframework.ezorm.rdb.executor;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.meta.expand.SimpleMapWrapper;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.Collections;
import java.util.Map;

import static org.hswebframework.ezorm.rdb.executor.SqlRequest.*;
import static org.junit.Assert.*;

public class JdbcSyncSqlExecutorTest {


    private SyncSqlExecutor executor;

    @SneakyThrows
    @Before
    public void init() {
        Class.forName("org.h2.Driver");
        Connection connection = DriverManager.getConnection("jdbc:h2:mem:hsweb", "sa", "");
        executor = new JdbcSyncSqlExecutor() {
            @Override
            public Connection getConnection() {
                return connection;
            }

            @Override
            public void releaseConnection(Connection connection) {

            }
        };
    }

    @SneakyThrows
    @Test
    public void testSelectSingle() {
        Map<String, Object> data = executor.selectSingle(of("select 1 as data"), SimpleMapWrapper.INSTANCE);
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

    @SneakyThrows
    @Test
    public void testSelectPrepare() {
        Map<String, Object> data = executor.selectSingle(of("select 1 as data where 1 = ?", 1), SimpleMapWrapper.INSTANCE);
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

    @SneakyThrows
    @Test
    public void testSelectTemplate() {
        Map<String, Object> data = executor.selectSingle(template("select 1 as data where 1 = #{id}", Collections.singletonMap("id", 1)), SimpleMapWrapper.INSTANCE);
        Assert.assertNotNull(data);
        Assert.assertEquals(data.get("DATA"), 1);
    }

}