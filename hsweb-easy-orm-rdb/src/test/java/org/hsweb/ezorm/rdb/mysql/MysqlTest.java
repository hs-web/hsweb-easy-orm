package org.hsweb.ezorm.rdb.mysql;

import org.hsweb.ezorm.core.dsl.Update;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.core.param.UpdateParam;
import org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.Correlation;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.parser.MysqlTableMetaParser;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.dialect.MysqlRDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;
import org.hsweb.ezorm.rdb.RDBDatabase;
import org.hsweb.ezorm.rdb.RDBTable;
import org.hsweb.ezorm.rdb.simple.SimpleDatabase;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class MysqlTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("com.mysql.jdbc.Driver");

        Connection connection = DriverManager.getConnection("jdbc:mysql://127.0.0.1:3306/hsweb?useUnicode=true&characterEncoding=utf-8&useSSL=false", "root", "19920622");
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
        RDBDatabaseMetaData databaseMetaData = new MysqlRDBDatabaseMetaData();
        databaseMetaData.setParser(new MysqlTableMetaParser(sqlExecutor));
        SimpleDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.setAutoParse(true);
        database.getTable("s_classified");
        RDBTable s_form = database.getTable("s_form");

        Correlation correlation = new Correlation();
        correlation.setTargetTable("s_classified");
        correlation.addTerm(new SqlTerm("s_classified.u_id=s_form.classified_id"));
        s_form.getMeta().addCorrelation(correlation);

        String sql = databaseMetaData.getRenderer(SqlRender.TYPE.SELECT).render(s_form.getMeta(), new QueryParam()).getSql();
        System.out.println(sql);
    }

    @Test
    public void testExec() throws Exception {
        RDBDatabaseMetaData databaseMetaData = new MysqlRDBDatabaseMetaData();
        RDBDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.createOrAlter("s_user")
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