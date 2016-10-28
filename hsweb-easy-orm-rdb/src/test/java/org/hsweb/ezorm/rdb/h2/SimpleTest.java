package org.hsweb.ezorm.rdb.h2;

import org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.render.dialect.H2DatabaseMeta;
import org.hsweb.ezorm.rdb.run.RDBDatabase;
import org.hsweb.ezorm.rdb.run.RDBTable;
import org.hsweb.ezorm.rdb.run.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public class SimpleTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("org.h2.Driver");
        Connection connection = DriverManager.getConnection("jdbc:h2:mem:hsweb", "sa", "");

        sqlExecutor = new AbstractJdbcSqlExecutor() {
            @Override
            public Connection getConnection() {
                return connection;
            }

            @Override
            public void releaseConnection(Connection connection) throws SQLException {
                //connection.close();
            }
        };
    }

    @Test
    public void testExec() throws Exception {
        RDBDatabaseMetaData databaseMetaData = new H2DatabaseMeta();
        RDBDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.createOrAlter("s_user")
                .addColumn().name("id").primaryKey().jdbcType(JDBCType.VARCHAR).length(32).comment("ID").commit()
                .addColumn().name("name").notNull().jdbcType(JDBCType.VARCHAR).length(256).comment("姓名").commit()
                .addColumn().name("age").notNull().jdbcType(JDBCType.NUMERIC).length(4, 2).comment("年龄").commit()
                .addColumn().name("create_date").jdbcType(JDBCType.TIMESTAMP).comment("创建时间").commit()
                .comment("用户表")
                .commit();
        RDBTable<Map<String, Object>> table = database.getTable("s_user");
        List<Map<String, Object>> aa =
                table.createQuery().where("name", "1").and("name", "2")
                        .nest().nest()
                        .like("name", "3").like("name", "4").like("name", "5").end()
                        .orNest().like("name", "6").like("name", "7").like("name", "8").end()
                        .end()
                        .and()
                        .between("age", 18, 28).list(0, 10);

        database.createOrAlter("s_user")
                .addColumn().name("id").primaryKey().jdbcType(JDBCType.VARCHAR).length(32).comment("ID").commit()
                .addColumn().name("name").notNull().jdbcType(JDBCType.VARCHAR).length(256).comment("姓名").commit()
                .addColumn().name("age").notNull().jdbcType(JDBCType.NUMERIC).length(4, 2).comment("年龄").commit()
                .addColumn().name("create_date").jdbcType(JDBCType.TIMESTAMP).comment("创建时间").commit()
                .addColumn().name("update_date").jdbcType(JDBCType.TIMESTAMP).comment("修改时间").commit()
                .comment("用户表")
                .commit();

//        table.createQuery().where("name", "aa").or().like("name", "aa").list();

//        table.createUpdate().set("name", "aaa").where("name", "aa").or().like("name", 1).exec();

    }

}