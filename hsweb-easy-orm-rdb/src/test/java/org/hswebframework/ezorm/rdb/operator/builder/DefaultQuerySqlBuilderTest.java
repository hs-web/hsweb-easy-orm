package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.meta.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.SimpleSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.query.BuildParameterQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.Orders;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.operator.dml.query.Orders.*;

public class DefaultQuerySqlBuilderTest {


    RDBSchemaMetadata schema;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        schema = new RDBSchemaMetadata();
        schema.setName("DEFAULT");

        database.setCurrentSchema(schema);
        database.addSchema(schema);

        RDBTableMetadata table = new RDBTableMetadata();
        table.setName("test");
        RDBTableMetadata detail = new RDBTableMetadata();
        detail.setName("detail");

        schema.addTable(table);
        schema.addTable(detail);

        RDBColumnMetadata id = new RDBColumnMetadata();
        id.setName("id");
        id.setJdbcType(JDBCType.VARCHAR);
        id.setLength(32);

        RDBColumnMetadata name = new RDBColumnMetadata();
        name.setName("name");
        name.setJdbcType(JDBCType.VARCHAR);
        name.setLength(64);

        table.addColumn(id);
        table.addColumn(name);

        RDBColumnMetadata detailInfo = new RDBColumnMetadata();
        detailInfo.setName("comment");
        detailInfo.setJdbcType(JDBCType.VARCHAR);
        detailInfo.setLength(64);

        detail.addColumn(detailInfo);

    }


    @Test
    public void test() {
        BuildParameterQueryOperator query = new BuildParameterQueryOperator();

//        schema.addFeature(new SqlServerPaginator());

        query.select("*")
                .from("test")
//                .leftJoin("detail", join -> join.as("info").on("test.id=info.id"))
//                .where(dsl -> dsl.is("name", "1234").is("info.comment", "1234"))
                .orderBy(Orders.count("name").asc(), desc("info.comment"))
//                .forUpdate()
                .paging(0, 10);

        DefaultQuerySqlBuilder sqlBuilder = new DefaultQuerySqlBuilder(schema);

        long time = System.currentTimeMillis();

        SqlRequest sqlRequest =  sqlBuilder.build(query.getParameter());

        System.out.println(System.currentTimeMillis() - time);

        Assert.assertNotNull(sqlRequest);
        Assert.assertNotNull(sqlRequest.getSql());
        System.out.println(((SimpleSqlRequest) sqlRequest).toNativeSql());

    }
}