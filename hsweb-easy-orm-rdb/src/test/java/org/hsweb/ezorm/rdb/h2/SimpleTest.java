package org.hsweb.ezorm.rdb.h2;

import com.alibaba.fastjson.JSON;
import org.hsweb.ezorm.core.dsl.ConditionColumnBuilder;
import org.hsweb.ezorm.core.dsl.Query;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.rdb.RDBDatabase;
import org.hsweb.ezorm.rdb.RDBTable;
import org.hsweb.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.converter.BlobValueConverter;
import org.hsweb.ezorm.rdb.meta.converter.ClobValueConverter;
import org.hsweb.ezorm.rdb.meta.parser.H2TableMetaParser;
import org.hsweb.ezorm.rdb.render.dialect.H2RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.function.Function;


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
        RDBDatabaseMetaData databaseMetaData = new H2RDBDatabaseMetaData();
        databaseMetaData.setParser(new H2TableMetaParser(sqlExecutor));
        RDBDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.createOrAlter("s_user")
                .addColumn().name("id").varchar(32).primaryKey().comment("id").commit()
                .addColumn().name("name").varchar(256).notNull().comment("姓名").commit()
                .addColumn().name("age").number(4).notNull().comment("年龄").commit()
                .addColumn().name("remark").clob().custom(column -> column.setValueConverter(new ClobValueConverter())).comment("备注").commit()
                .addColumn().name("photo").jdbcType(JDBCType.CLOB).custom(column -> column.setValueConverter(new BlobValueConverter())).comment("照片").commit()
                .addColumn().name("create_date").datetime().comment("创建时间").commit()
                .comment("用户表")
                .commit();

        RDBTable<Map<String, Object>> table = database.getTable("s_user");

//        List<Map<String, Object>> aa =
//                table.createQuery().where("name", "1").and("name", "2")
//                        .nest().nest()
//                        .like("name", "3").like("name", "4").like("name", "5").end()
//                        .orNest().like("name", "6").like("name", "7").like("name", "8").end().or().in("age", "1,2,3")
//                        .end()
//                        .and()
//                        .between("age", 18, 28).list(0, 10);

//
//        QueryParam queryParam = new Query<>(new QueryParam())
//                .fromBean(Collections.singletonMap("name", "aa"))
//                .like("name").like("name")
//                .nest().like("name").end()
//                .getParam();
//
//
//        table.createQuery().setParam(queryParam).list();

        table.createInsert().value(JSON.parseObject("{\n" +
                "  \"id\": \"test\",\n" +
                "  \"name\": \"测试\",\n" +
                "  \"age\": 10,\n" +
                "  \"photo\":\"test123\",\n" +
                "  \"remark\": \"测试123\"\n" +
                "}")).exec();
        System.out.println(table.createQuery().list());

        Function<Object, Object> append = (value) -> "," + value + ",";
        table.createQuery()
                .where()
                .sql("age >? or age <?", 1, 5)
                .when("age", 10, age -> age > 10, query -> query.or()::like)
                .each(Collections.singletonMap("name", "张三"), query -> query::$like$)
                .each(Collections.singletonMap("name", "张三"), "is", query -> query::and)
                .nest()
                .nest()
                .sql("age > 10")
                .sql("age > #{age}", Collections.singletonMap("age", 10))
                .sql("age > #{[0]} or age > #{[0]}", Arrays.asList(1, 2))
                .end()
                .or().sql("name = ?", "' or '1'='1")
                .end()
                .nest()
                .or()
                .each("age", Arrays.asList(1, 2, 3), query -> query::$like$, append)
                .each(Arrays.asList(4, 5, 6), (q, o) -> q.like("age", o))
                .end()
                .list();

        table.createQuery().where(
                ConditionColumnBuilder.build("name")
                        .like("111").andThen(ConditionColumnBuilder.build("name").$like$("123"))).list();

        //                  where name = '张三'  (      age > 10     or     age < 5   )
//        table.createQuery().where("name", "张三").nest().gt("age", 10).or().lt("age", 5).end().list();

        table.createDelete().where().notNull("age").exec();
        database.createOrAlter("s_user")
                .addOrAlterColumn("age").number(5).notNull().comment("年龄").commit()
                .addColumn().name("update_date").datetime().comment("修改时间").commit()
                .removeColumn("photo")
                .comment("用户表")
                .commit();

//        table.createQuery().where("name", "aa").or().like("name", "aa").list();

//        table.createUpdate().set("name", "aaa").where("name", "aa").or().like("name", 1).exec();

        sqlExecutor.list("select * from s_user where age > #{age}",Collections.singletonMap("age",10));
    }

}