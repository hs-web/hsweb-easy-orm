package og.hsweb.ezorm.test;

import og.hsweb.ezorm.executor.AbstractJdbcSqlExecutor;
import og.hsweb.ezorm.executor.SqlExecutor;
import og.hsweb.ezorm.meta.Correlation;
import og.hsweb.ezorm.meta.DatabaseMetaData;
import og.hsweb.ezorm.meta.TableMetaData;
import og.hsweb.ezorm.meta.expand.ObjectWrapper;
import og.hsweb.ezorm.meta.expand.ObjectWrapperFactory;
import og.hsweb.ezorm.meta.expand.Validator;
import og.hsweb.ezorm.meta.expand.ValidatorFactory;
import og.hsweb.ezorm.meta.parser.OracleTableMetaParser;
import og.hsweb.ezorm.render.dialect.OracleDatabaseMeta;
import og.hsweb.ezorm.run.Table;
import og.hsweb.ezorm.run.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * Created by zhouhao on 16-6-5.
 */
public class AutoParserTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("oracle.jdbc.driver.OracleDriver");
        sqlExecutor = new AbstractJdbcSqlExecutor() {
            @Override
            public Connection getConnection() {
                try {
                    return DriverManager.getConnection("jdbc:oracle:thin:@127.0.0.1:1521:XE", "data_center", "data_center");
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
        DatabaseMetaData metaData = new OracleDatabaseMeta();
        SimpleDatabase database = new SimpleDatabase(metaData, sqlExecutor);
        database.setTableMetaParser(new OracleTableMetaParser(sqlExecutor, "data_center"));
        Table user = database.getTable("s_user");
        Table resources = database.getTable("s_resources");
        //设置表关联
        resources.getMeta().addCorrelation(
                new Correlation("s_user", "creator", "creator.u_id=s_resources.creator_id").leftJoin()
        );

        System.out.println(resources.createQuery()
                .select("u_id", "name", "creator.username")
                .list(0, 1));
    }
}
