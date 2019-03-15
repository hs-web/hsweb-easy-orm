package org.hswebframework.ezorm.rdb.h2;

import com.alibaba.fastjson.JSON;
import org.apache.commons.codec.digest.DigestUtils;
import org.hswebframework.ezorm.core.dsl.ConditionColumnBuilder;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.RDBDatabase;
import org.hswebframework.ezorm.rdb.RDBTable;
import org.hswebframework.ezorm.rdb.executor.AbstractJdbcSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlExecutor;
import org.hswebframework.ezorm.rdb.meta.Correlation;
import org.hswebframework.ezorm.rdb.meta.converter.BlobValueConverter;
import org.hswebframework.ezorm.rdb.meta.converter.ClobValueConverter;
import org.hswebframework.ezorm.rdb.meta.converter.DateTimeConverter;
import org.hswebframework.ezorm.rdb.meta.parser.H2TableMetaParser;
import org.hswebframework.ezorm.rdb.render.dialect.DefaultDialect;
import org.hswebframework.ezorm.rdb.render.dialect.H2RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.simple.SimpleDatabase;
import org.junit.Before;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.JDBCType;
import java.sql.SQLException;
import java.util.*;
import java.util.function.Function;


public class SimpleTest {
    SqlExecutor sqlExecutor;

    @Before
    public void setup() throws Exception {
       System.setProperty("easyorm.paging.prepare","true");

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

        H2RDBDatabaseMetaData databaseMetaData = new H2RDBDatabaseMetaData();
        databaseMetaData.setDatabaseName("PUBLIC");
        H2TableMetaParser metaParser=new H2TableMetaParser(sqlExecutor);

        databaseMetaData.setParser(metaParser);
        metaParser.setDatabaseName("PUBLIC");

        RDBDatabase database = new SimpleDatabase(databaseMetaData, sqlExecutor);
        database.createOrAlter("s_user_info")
                .addColumn().name("id").varchar(32).primaryKey().comment("id").commit()
                .addColumn().name("address").columnDef("varchar(32) not null comment '地址'").commit()
                .addColumn().name("id_card").alias("idCard").varchar(32).notNull().comment("身份证号").commit()
                .comment("用户明细表")
                .commit();

        database.createOrAlter("s_user")
                .addColumn().name("id").varchar(32).primaryKey().comment("id")
                .custom(column -> column.setDefaultValue(() -> DigestUtils.md5Hex(UUID.randomUUID().toString()))).commit()
                .addColumn().name("name").varchar(256).notNull().comment("姓名").commit()
                .addColumn().name("age").number(4).notNull().comment("年龄").commit()
                .addColumn().name("remark").clob().custom(column -> column.setValueConverter(new ClobValueConverter())).comment("备注").commit()
                .addColumn().name("photo").jdbcType(JDBCType.CLOB).custom(column -> column.setValueConverter(new BlobValueConverter())).comment("照片").commit()
                .addColumn().name("create_date").datetime().custom(columnMetaData -> columnMetaData.setValueConverter(new DateTimeConverter("yyyy-MM-dd HH:mm:ss", Date.class))).comment("创建时间").commit()
                //表关联
                .custom(table -> table.addCorrelation(new Correlation("s_user_info", "info", "info.id=s_user.id")))
                .comment("用户表")
                .index()
                .name("test_index")
                .column("name")
                .unique()
                .commit()
                .commit();


        RDBTable<Map<String, Object>> user = database.getTable("s_user");
        RDBTable<Map<String, Object>> info = database.getTable("s_user_info");


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
//        table.createQuery().setParams(queryParam).list();

        user.createInsert().values((Collection) JSON.parseArray("[" +
                "{\n" +
                "  \"name\": \"测试\",\n" +
                "  \"age\": 10,\n" +
                "  \"photo\":\"test123\",\n" +
                "  \"remark\": \"测试123\"\n" +
                "}," +
                "{\n" +
                "  \"id\": \"test2\",\n" +
                "  \"name\": \"测试2\",\n" +
                "  \"age\": 11,\n" +
                "  \"photo\":\"test123\",\n" +
                "  \"remark\": \"测试123\"\n" +
                "}" +
                "]")).exec();

        info.createInsert().value(JSON.parseObject(
                "{\n" +
                        "  \"id\": \"test2\",\n" +
                        "  \"address\": \"测试地址\",\n" +
                        "  \"id_card\":\"123123132\"\n" +
                        "}")).exec();

        System.out.println(user.createQuery().list());

        Function<Object, Object> append = (value) -> "," + value + ",";


        List<Map<String, Object>> age1 = user.createQuery().notIn("age", 12, 11).list();
        System.out.println("age1" + age1);

        List<Map<String, Object>> age2 = user.createQuery().notIn("age", 11).list();
        System.out.println("age2" + age2);

        List<Map<String, Object>> age3 = user.createQuery().notIn("age", Arrays.asList(1, 11)).list();

        System.out.println("age3" + age3);

        user.createQuery().select("id")
                .where()
                .is("name", "张三")
                .nest()
                .or()
                .$like$("info.address", "测试")
                .like("name2", "李%")
                .end()
                .list(0,10);

        user.createQuery()
                .where()
                .accept("create_date", TermType.btw, "2017-10-01,2017-10-10")
                .sql("age >? or age <?", 1, 5)
                .when(() -> true, query -> query.like("age", 10))
                .when("age", 10, age -> age > 10, query -> query.or()::like)
                .each(Collections.singletonMap("name", "张三"), query -> query::$like$)
                .each(Collections.singletonMap("name", "张三"), "is", query -> query::and)
                .nest()
                .nest()
                .and("name$like$reverse$endWith", "1234")
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

        //                  where name = '张三'  (      age > 10     or     age < 5   )
//        table.createQuery().where("name", "张三").nest().gt("age", 10).or().lt("age", 5).end().list();

        user.createDelete().where().notNull("age").exec();

        database.createOrAlter("s_user")
                .addOrAlterColumn("age").number(5).notNull().comment("年龄").commit()
                .addColumn().name("update_date").datetime().comment("修改时间").commit()
                .removeColumn("photo")
                .comment("用户表")
                .commit();

//        table.createQuery().where("name", "aa").or().like("name", "aa").list();

//        table.createUpdate().set("name", "aaa").where("name", "aa").or().like("name", 1).exec();

        sqlExecutor.list("select * from s_user where age > #{age}", Collections.singletonMap("age", 10));
    }


    static class Bean {
        private String name;

        public String getName() {
            return name;
        }
    }

}