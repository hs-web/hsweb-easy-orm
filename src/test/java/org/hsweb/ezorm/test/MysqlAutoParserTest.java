package org.hsweb.ezorm.test;

import org.hsweb.ezorm.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.Correlation;
import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.parser.MysqlTableMetaParser;
import org.hsweb.ezorm.meta.parser.OracleTableMetaParser;
import org.hsweb.ezorm.render.dialect.MysqlDatabaseMeta;
import org.hsweb.ezorm.render.dialect.OracleDatabaseMeta;
import org.hsweb.ezorm.run.Table;
import org.hsweb.ezorm.run.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

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
    public void testParser() throws SQLException {
        DatabaseMetaData metaData = new MysqlDatabaseMeta();
        SimpleDatabase database = new SimpleDatabase(metaData, sqlExecutor);
        metaData.setParser(new MysqlTableMetaParser(sqlExecutor));
        metaData.init();

        Table user = database.getTable("s_user");

        user.createQuery().select("username").where("name$like", "张%").list();

        Table resources = database.getTable("s_resources");
        //设置表关联
        resources.getMeta().addCorrelation(
                new Correlation("s_user", "creator", "creator.u_id=s_resources.creator_id").leftJoin()
        );
        resources.createUpdate().includes("name").set("name", "111").where("u_id", "aa").exec();
        resources.createDelete().where("u_id", "11").exec();

        System.out.println(user.createQuery()
                .select("u_id", "name", "creator.username").single());
    }

    @Test
    public void testAlter() throws Exception {
        DatabaseMetaData metaData = new OracleDatabaseMeta();
        SimpleDatabase database = new SimpleDatabase(metaData, sqlExecutor);
        metaData.setParser(new OracleTableMetaParser(sqlExecutor));
        metaData.init();
        Table script = database.getTable("db_food_info");
        TableMetaData metaData1 = script.getMeta().clone();

//      metaData1.findFieldByName("model").setDataType("varchar2(128)");
        metaData1.removeField("model");
        metaData1.setComment("服务端脚本");
        database.alterTable(metaData1);

    }
}
