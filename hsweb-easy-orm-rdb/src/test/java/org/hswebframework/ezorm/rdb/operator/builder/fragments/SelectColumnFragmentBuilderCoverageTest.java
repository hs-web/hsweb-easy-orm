package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.metadata.key.ForeignKeyBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.query.SelectColumnFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.NativeSelectColumn;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;
import java.util.Set;

public class SelectColumnFragmentBuilderCoverageTest {

    private RDBSchemaMetadata schema;
    private RDBTableMetadata table;
    private RDBTableMetadata detail;
    private RDBTableMetadata detail2;
    private SelectColumnFragmentBuilder builder;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        schema = new RDBSchemaMetadata("DEFAULT");
        database.setCurrentSchema(schema);
        database.addSchema(schema);
        table = schema.newTable("test");
        detail = schema.newTable("detail");
        detail2 = schema.newTable("detail2");
        schema.addTable(table);
        schema.addTable(detail);
        schema.addTable(detail2);
        addColumn(table, "id");
        addColumn(table, "name");
        addColumn(detail, "comment");
        addColumn(detail2, "comment");
        table.addForeignKey(ForeignKeyBuilder.builder().target("detail").autoJoin(true).build().addColumn("id", "comment"));
        builder = SelectColumnFragmentBuilder.of(table);
    }

    @Test
    public void testDefaultAndExcludesAndAliasBranches() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.getSelectExcludes().add("name");
        Assert.assertEquals("test.\"ID\" as \"id\"", builder.createFragments(parameter).toRequest().getSql());

        QueryOperatorParameter aliasParameter = new QueryOperatorParameter();
        aliasParameter.setFrom("test");
        aliasParameter.getAlias().add(SelectColumn.of("name", "displayName"));
        aliasParameter.getWhere().add(org.hswebframework.ezorm.core.param.Term.of("displayName", "eq", "JetLinks"));
        String aliasSql = builder.createFragments(aliasParameter).toRequest().getSql();
        Assert.assertTrue(aliasSql.contains("test.\"ID\" as \"id\""));
        Assert.assertTrue(aliasSql.contains("test.\"NAME\" as \"name\""));

        QueryOperatorParameter nativeParameter = new QueryOperatorParameter();
        NativeSelectColumn nativeColumn = new NativeSelectColumn("coalesce(?, test.\"NAME\")", new Object[]{"fallback"});
        nativeColumn.setAlias("displayName");
        nativeParameter.setSelect(Collections.singletonList(nativeColumn));
        Assert.assertEquals("coalesce(?, test.\"NAME\") as \"DISPLAYNAME\"", builder.createFragments(nativeParameter).toRequest().getSql());
    }

    @Test
    public void testJoinWildcardAutoJoinAndUnknownBranch() {
        Join join = new Join();
        join.setTarget("detail2");
        join.setAlias("info");

        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.getJoins().add(join);
        parameter.setSelect(Arrays.asList(SelectColumn.of("*") , SelectColumn.of("detail.*"), SelectColumn.of("info.comment"), SelectColumn.of("unknown.*")));
        String sql = builder.createFragments(parameter).toRequest().getSql();
        Assert.assertTrue(sql.contains("test.\"ID\""));
        Assert.assertTrue(sql.contains("detail.\"COMMENT\""));
        Assert.assertTrue(sql.contains("info.\"COMMENT\""));
    }

    @Test
    public void testFunctionBranchesAndFallbackAlias() {
        table.addFeature(new org.hswebframework.ezorm.rdb.operator.builder.fragments.function.SimpleFunctionFragmentBuilder("sum", "sum"));
        SelectColumn sum = new SelectColumn();
        sum.setColumn("id");
        sum.setAlias("totalId");
        sum.setFunction("sum");
        sum.setOpts(Collections.singletonMap("distinct", true));
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setSelect(Collections.singletonList(sum));
        Assert.assertTrue(builder.createFragments(parameter).toRequest().getSql().contains("sum( distinct test.\"ID\" ) as \"totalId\""));

        SelectColumn unknownFunction = new SelectColumn();
        unknownFunction.setColumn("id");
        unknownFunction.setFunction("missing_function");
        parameter.setSelect(Collections.singletonList(unknownFunction));
        Assert.assertTrue(builder.createFragments(parameter).isEmpty());
    }

    private void addColumn(RDBTableMetadata table, String name) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        table.addColumn(column);
    }
}
