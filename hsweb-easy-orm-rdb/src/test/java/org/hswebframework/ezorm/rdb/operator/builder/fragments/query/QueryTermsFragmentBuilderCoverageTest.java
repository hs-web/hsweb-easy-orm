package org.hswebframework.ezorm.rdb.operator.builder.fragments.query;

import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.JdbcDataType;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.dml.Join;
import org.hswebframework.ezorm.rdb.operator.dml.query.QueryOperatorParameter;
import org.hswebframework.ezorm.rdb.operator.dml.query.SelectColumn;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Collections;
import java.util.Set;

public class QueryTermsFragmentBuilderCoverageTest {

    private RDBTableMetadata table;
    private RDBTableMetadata detail;

    @Before
    public void init() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(Dialect.H2);
        RDBSchemaMetadata schema = new RDBSchemaMetadata("PUBLIC");
        database.setCurrentSchema(schema);
        database.addSchema(schema);
        table = schema.newTable("test");
        detail = schema.newTable("detail");
        schema.addTable(table);
        schema.addTable(detail);
        addColumn(table, "id");
        addColumn(table, "name");
        addColumn(detail, "comment");
    }

    @Test
    public void testJoinAliasAndTableAliasTerms() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setFrom("test");
        Join join = new Join();
        join.setTarget("detail");
        join.setAlias("info");
        parameter.getJoins().add(join);
        parameter.getWhere().add(Term.of("info.comment", "eq", "ok"));

        SqlRequest joinRequest = QueryTermsFragmentBuilder.of(table).createFragments(parameter).toRequest();
        Assert.assertEquals("info.\"COMMENT\" = ?", joinRequest.getSql());

        QueryOperatorParameter aliasParameter = new QueryOperatorParameter();
        aliasParameter.setFrom("test");
        aliasParameter.getWhere().add(Term.of("t.name", "eq", "JetLinks"));
        SqlRequest aliasRequest = QueryTermsFragmentBuilder.of(table, Set.of("t")).createFragments(aliasParameter).toRequest();
        Assert.assertEquals("test.\"NAME\" = ?", aliasRequest.getSql());
    }

    @Test
    public void testSelectAliasListAndNullColumnBranches() {
        QueryOperatorParameter parameter = new QueryOperatorParameter();
        parameter.setFrom("test");
        parameter.getAlias().add(SelectColumn.of("name", "displayName"));
        parameter.getWhere().add(Term.of("displayName", "eq", "JetLinks"));
        Assert.assertEquals("test.\"NAME\" = ?", QueryTermsFragmentBuilder.of(table).createFragments(parameter).toRequest().getSql());

        QueryOperatorParameter nullColumn = new QueryOperatorParameter();
        nullColumn.setFrom("test");
        nullColumn.getWhere().add(Term.of(null, "eq", "ignored"));
        Assert.assertTrue(QueryTermsFragmentBuilder.of(table).createFragments(nullColumn).isEmpty());
    }

    @Test
    public void testUnknownColumnStrictAndNonStrictBranches() {
        QueryOperatorParameter nonStrict = new QueryOperatorParameter();
        nonStrict.setFrom("test");
        nonStrict.getWhere().add(Term.of("missing", "eq", "ignored"));
        Assert.assertTrue(QueryTermsFragmentBuilder.of(table).createFragments(nonStrict).isEmpty());

        QueryOperatorParameter strict = new QueryOperatorParameter();
        strict.setFrom("test");
        strict.setContext(Collections.singletonMap(QueryOperatorParameter.STRICT_TERM_KEY, true));
        strict.getWhere().add(Term.of("missing", "eq", "ignored"));
        try {
            QueryTermsFragmentBuilder.of(table).createFragments(strict).toRequest();
            Assert.fail("strict mode should reject unsupported term");
        } catch (UnsupportedOperationException expected) {
            Assert.assertTrue(expected.getMessage().contains("Unsupported term"));
        }

        QueryOperatorParameter joinStrict = new QueryOperatorParameter();
        joinStrict.setFrom("test");
        Join join = new Join();
        join.setTarget("detail");
        join.setAlias("info");
        joinStrict.getJoins().add(join);
        joinStrict.setContext(Collections.singletonMap(QueryOperatorParameter.STRICT_TERM_KEY, true));
        joinStrict.getWhere().add(Term.of("info.missing", "eq", "ignored"));
        try {
            QueryTermsFragmentBuilder.of(table).createFragments(joinStrict).toRequest();
            Assert.fail("strict join branch should reject unsupported join term");
        } catch (UnsupportedOperationException expected) {
            Assert.assertTrue(expected.getMessage().contains("join or foreign key"));
        }
    }

    private void addColumn(RDBTableMetadata table, String name) {
        RDBColumnMetadata column = table.newColumn();
        column.setName(name);
        column.setType(JdbcDataType.of(JDBCType.VARCHAR, String.class));
        table.addColumn(column);
    }
}
