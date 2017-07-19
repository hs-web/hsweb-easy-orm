package org.hsweb.ezorm.rdb.sqlserver;

import com.alibaba.fastjson.JSON;
import org.hsweb.ezorm.rdb.RDBDatabase;
import org.hsweb.ezorm.rdb.RDBTable;
import org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.parser.SqlServer2012TableMetaParser;
import org.hsweb.ezorm.rdb.render.dialect.MSSQLRDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.simple.SimpleDatabase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public class SqlServerTests {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("com.microsoft.sqlserver.jdbc.SQLServerDriver");

        Connection connection = DriverManager
                .getConnection("jdbc:sqlserver://192.168.1.9:1433;DatabaseName=hsweb;integratedSecurity=false"
                        , "sa", "hsweb_sqlserver2012");
        sqlExecutor = new AbstractJdbcSqlExecutor() {
            @Override
            public Connection getConnection() {
                return connection;
            }

            @Override
            public void releaseConnection(Connection connection) throws SQLException {
                // connection.close();
            }
        };
    }

    @Test
    public void testParser() throws Exception {
        RDBDatabaseMetaData databaseMetaData = new MSSQLRDBDatabaseMetaData();
        databaseMetaData.setParser(new SqlServer2012TableMetaParser(sqlExecutor));
        SimpleDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.setAutoParse(true);
        RDBTable<Map<String, Object>> s_test = database.getTable("s_test");

        List<Map<String, Object>> data = s_test.createQuery().where("name", "test").orderByDesc("u_id").list(0, 20);

        System.out.println(data);
    }

    @Test
    public void testExec() throws Exception {
        RDBDatabaseMetaData databaseMetaData = new MSSQLRDBDatabaseMetaData();
        RDBDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.createOrAlter("s_user")
                .addColumn().name("id").varchar(32).primaryKey().comment("id").commit()
                .addColumn().name("name").varchar(256).notNull().comment("姓名").commit()
                .addColumn().name("age").number(4).notNull().comment("年龄").commit()
                .addColumn().name("create_date").datetime().comment("创建时间").commit()
                .comment("用户表")
                .commit();
        RDBTable<Map<String, Object>> table = database.getTable("s_user");
        table.createDelete().where().is("id","test").exec();
        table.createInsert().value(JSON.parseObject("{'id':'test','name':'测试','age':10}")).exec();
        List<Map<String, Object>> aa =
                table.createQuery().where("name", "1").and("name", "2")
                        .nest().nest()
                        .like("name", "3").like("name", "4").like("name", "5").end()
                        .orNest().like("name", "6").like("name", "7").like("name", "8").end()
                        .end()
                        .and()
                        .between("age", 18, 28)
                        .list(0, 10);

        System.out.println(aa);

        database.createOrAlter("s_user")
                .addColumn().name("id").varchar(32).primaryKey().comment("id").commit()
                .addColumn().name("name").varchar(256).notNull().comment("姓名").commit()
                .addColumn().name("age").number(4).notNull().comment("年龄").commit()
                .addColumn().name("create_date").datetime().comment("创建时间").commit()
                .addColumn().name("update_date").datetime().comment("修改时间").commit()
                .comment("用户表")
                .commit();
    }

    @After
    public void after() throws SQLException {
        // sqlExecutor.exec(new SimpleSQL("drop table s_user"));
    }
}
