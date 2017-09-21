package org.hswebframework.ezorm.rdb.oracle;

import org.hswebframework.ezorm.core.dsl.Update;
import org.hswebframework.ezorm.core.param.UpdateParam;
import org.hswebframework.ezorm.rdb.RDBTable;
import org.hswebframework.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.meta.parser.OracleTableMetaParser;
import org.hswebframework.ezorm.rdb.render.dialect.OracleRDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.simple.SimpleDatabase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * TODO 完成注释
 *
 * @author zhouhao
 */
public class OralceTests {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("oracle.jdbc.driver.OracleDriver");

        Connection connection = DriverManager
                .getConnection(
                        "jdbc:oracle:thin:@127.0.0.1:1521:orcl",
                        "test",
                        "test");
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
    public void testExec() throws Exception {
        RDBDatabaseMetaData databaseMetaData = new OracleRDBDatabaseMetaData();
        databaseMetaData.setParser(new OracleTableMetaParser(sqlExecutor));
        SimpleDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.setAutoParse(true);
        database.createOrAlter("s_demo")
                .addColumn().name("id").varchar(32).primaryKey().comment("id").commit()
                .addColumn().name("name").varchar(256).notNull().comment("姓名").commit()
                .addColumn().name("age").number(4).notNull().comment("年龄").commit()
                .addColumn().name("create_date").datetime().comment("创建时间").commit()
                .comment("用户表")
                .commit();
        RDBTable<Map<String, Object>> table = database.getTable("s_user");

        new Update<>(new UpdateParam<>())
                .setExecutor((param) -> {
                    try {
                        return table.createUpdate().setParam(param).exec();
                    } catch (SQLException e) {
                    }
                    return 0;
                })
                .fromBean(Collections.singletonMap("name", "aa"))
                .where("name")
                .nest().like("name").end()
                .exec();

        List<Map<String, Object>> aa =
                table.createQuery().where("name", "1").and("name", "2")
                        .nest().nest()
                        .like("name", "3").like("name", "4").like("name", "5").end()
                        .orNest().like("name", "6").like("name", "7").like("name", "8").end()
                        .end()
                        .and()
                        .between("age", 18, 28)
                        .list(0, 10);

        database.createOrAlter("s_demo")
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
        sqlExecutor.exec("drop table s_demo");
    }
}
