package org.hsweb.ezorm.executor;

import com.alibaba.fastjson.JSON;
import org.hsweb.ezorm.meta.Correlation;
import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.meta.FieldMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.expand.Trigger;
import org.hsweb.ezorm.meta.parser.H2TableMetaParser;
import org.hsweb.ezorm.param.Term;
import org.hsweb.ezorm.param.TermType;
import org.hsweb.ezorm.render.dialect.H2DatabaseMeta;
import org.hsweb.ezorm.render.dialect.OracleDatabaseMeta;
import org.hsweb.ezorm.run.Database;
import org.hsweb.ezorm.run.Query;
import org.hsweb.ezorm.run.Table;
import org.hsweb.ezorm.run.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.*;

/**
 * Created by zhouhao on 16-6-4.
 */
public class SimpleTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
        Class.forName("org.h2.Driver");
        sqlExecutor = new AbstractJdbcSqlExecutor() {
            @Override
            public Connection getConnection() {
                try {
                    return DriverManager.getConnection("jdbc:h2:mem:hsweb", "sa", "");
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
    public void testExec() throws Exception {
        DatabaseMetaData databaseMetaData = new H2DatabaseMeta();
        TableMetaData metaData = new TableMetaData();
        metaData.setName("s_user");
        metaData.setAlias("user");
        Correlation correlation = new Correlation();
        correlation.setTargetTable("s_area");
        correlation.setAlias("area1");
        Term term = new Term();
        term.setTermType(TermType.func);
        term.setField("area1.id");
        term.setValue("area1.id=user.area_id");
        correlation.setTerms(Arrays.asList(term));
        metaData.addCorrelation(correlation);

        correlation = new Correlation();
        correlation.setTargetTable("s_area");
        correlation.setAlias("area2");
        term = new Term();
        term.setTermType(TermType.func);
        term.setField("area2.id");
        term.setValue("area2.name=user.name");
        correlation.setTerms(Arrays.asList(term));
        metaData.addCorrelation(correlation);

        TableMetaData area = new TableMetaData();
        area.setName("s_area");
        area.setAlias("area");
        databaseMetaData.putTable(area);
        FieldMetaData area_id = new FieldMetaData();
        area_id.setName("id");
        area_id.setJavaType(String.class);
        area_id.setJdbcType(JDBCType.VARCHAR);
        area_id.setDataType("varchar(64)");
        FieldMetaData area_name = new FieldMetaData();
        area_name.setName("name");
        area_name.setJavaType(String.class);
        area_name.setJdbcType(JDBCType.VARCHAR);
        area_name.setDataType("varchar(64)");

        area.addField(area_id).addField(area_name);

        FieldMetaData fieldMetaData = new FieldMetaData();
        fieldMetaData.setName("user_name");
        fieldMetaData.setAlias("userName");
        fieldMetaData.setJavaType(String.class);
        fieldMetaData.setJdbcType(JDBCType.VARCHAR);
        fieldMetaData.setDataType("varchar(64)");
        FieldMetaData f2 = new FieldMetaData();
        f2.setName("name");
        f2.setJavaType(String.class);
        f2.setJdbcType(JDBCType.VARCHAR);
        f2.setDataType("varchar(64)");
        metaData.addField(fieldMetaData).addField(f2);

//        databaseMetaData.putTable(metaData);

//        databaseMetaData.putTable(area);

        databaseMetaData.init();

        metaData.on(Trigger.select_wrapper_done, context -> System.out.println(context.get("instance")));
        Database database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        area.setPrimaryKeys(new HashSet<>(Arrays.asList("id", "name")));
        database.createTable(metaData);
        database.createTable(area);

        Table<Map<String, Object>> table = database.getTable("s_user");
        List<Map<String, Object>> datas = JSON.parseObject("[{\"userName\":\"admin\",\"name\":\"张三\"},{\"userName\":\"admin2\",\"name\":\"张2\"}]", List.class);

        table.createInsert().values(datas).exec();

        Query<Map<String, Object>> query = table.createQuery();
        query.select("userName", "name", "area2.*")
                .where("name", "张三")
                .nest("name$LIKE", "张%").or("name$LIKE", "李%");
        query.orderByDesc("name");


        H2TableMetaParser parser = new H2TableMetaParser(sqlExecutor);
        TableMetaData metaData1 = parser.parse("s_user");
        metaData1.getFields().forEach(System.out::println);
        metaData1.findFieldByName("user_name").setName("test");
        metaData1.findFieldByName("user_name").setProperty("not-null", true);
        database.alterTable(metaData1);
        metaData1 = parser.parse("s_user");
        metaData1.findFieldByName("test").setProperty("not-null", false);
        database.alterTable(metaData1);
    }

    @Test
    public void testAutoParser() throws SQLException {
    }

}