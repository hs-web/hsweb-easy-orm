package org.hswebframework.ezorm.rdb.operator.builder;

import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.executor.SimpleSqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.meta.DefaultRDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.DefaultQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.Selects;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;

import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.*;
import static org.junit.Assert.*;

public class DefaultQuerySqlBuilderTest {


    DefaultRDBSchemaMetadata schema;

    @Before
    public void init() {
        DefaultRDBDatabaseMetadata<DefaultRDBSchemaMetadata> database = new DefaultRDBDatabaseMetadata<>(Dialect.H2);
        schema = new DefaultRDBSchemaMetadata();
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
        DefaultQueryOperator query = new DefaultQueryOperator();

        query.select("id")
                .select(select("info.comment").as("test"))
                .from("test")
                .leftJoin("detail",join->join.as("info").on("test.id=info.id"))
                .where(dsl -> dsl.is("name", "1234").is("info.comment","1234"));

        DefaultQuerySqlBuilder sqlBuilder = new DefaultQuerySqlBuilder(query.getParameter(), schema);

        SqlRequest sqlRequest = sqlBuilder.build();

        Assert.assertNotNull(sqlRequest);
        Assert.assertNotNull(sqlRequest.getSql());
        System.out.println(((SimpleSqlRequest) sqlRequest).toNativeSql());

    }
}