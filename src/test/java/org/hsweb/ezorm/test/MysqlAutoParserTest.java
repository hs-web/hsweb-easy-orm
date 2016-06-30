package org.hsweb.ezorm.test;

import org.hsweb.ezorm.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.Correlation;
import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.converter.ClobValueConverter;
import org.hsweb.ezorm.meta.expand.SimpleMapWrapper;
import org.hsweb.ezorm.meta.parser.MysqlTableMetaParser;
import org.hsweb.ezorm.meta.parser.OracleTableMetaParser;
import org.hsweb.ezorm.render.dialect.MysqlDatabaseMeta;
import org.hsweb.ezorm.render.dialect.OracleDatabaseMeta;
import org.hsweb.ezorm.render.support.simple.SimpleSQL;
import org.hsweb.ezorm.run.Table;
import org.hsweb.ezorm.run.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

/**
 * Created by zhouhao on 16-6-5.
 */
public class MysqlAutoParserTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("com.mysql.jdbc.Driver");
        sqlExecutor = new AbstractJdbcSqlExecutor() {
            @Override
            public Connection getConnection() {
                try {
                    Connection connection = DriverManager.getConnection("jdbc:mysql://127.0.0.1:3306/hsweb?useUnicode=true&characterEncoding=utf-8&useSSL=false", "root", "19920622");
                    connection.setAutoCommit(false);
                    return connection;
                } catch (SQLException e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            public void releaseConnection(Connection connection) throws SQLException {
                connection.close();
            }
        };
    }

    @Test
    public void testParseAll() throws SQLException {
//        List<TableMetaData> tableMetaDatas = new MysqlTableMetaParser(sqlExecutor).parseAll();
//        tableMetaDatas.forEach(tableMetaData -> System.out.println(tableMetaData.getName()));
        sqlExecutor.single(new SimpleSQL("select * from s_user where 1=2"), new SimpleMapWrapper() {
            @Override
            public void setUp(List<String> columns) {
                System.out.println(columns);
            }
        });
    }

    @Test
    public void testParser() throws SQLException {
        DatabaseMetaData metaData = new MysqlDatabaseMeta();
        SimpleDatabase database = new SimpleDatabase(metaData, sqlExecutor);
        metaData.setParser(new MysqlTableMetaParser(sqlExecutor));
        metaData.init();

        Table user = database.getTable("s_user");
        Table s_form = database.getTable("s_form");

        System.out.println(s_form.createQuery().select("html").where("using", 1).list());
        user.createQuery().select("username").where("name$like", "张%").list();

        Table resources = database.getTable("s_resources");
        //设置表关联
        resources.getMeta().addCorrelation(
                new Correlation("s_user", "creator", "creator.u_id=s_resources.creator_id").leftJoin()
        );
        resources.createUpdate().includes("name").set("name", "111").where("u_id", "aa").exec();
        resources.createDelete().where("u_id", "11").exec();

        System.out.println(user.createQuery().forUpdate().single());
    }

    @Test
    public void testAlter() throws Exception {
        DatabaseMetaData metaData = new MysqlDatabaseMeta();
        SimpleDatabase database = new SimpleDatabase(metaData, sqlExecutor);
        metaData.setParser(new MysqlTableMetaParser(sqlExecutor));
        metaData.init();
        TableMetaData old = metaData.getParser().parse("s_script");
        metaData.putTable(old);
        TableMetaData newTable = metaData.getParser().parse("s_script");
        newTable.findFieldByName("name2").setComment("test");
        database.alterTable(newTable);

    }

}
