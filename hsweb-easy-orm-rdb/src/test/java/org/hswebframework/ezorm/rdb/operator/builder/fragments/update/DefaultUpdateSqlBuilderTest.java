package org.hswebframework.ezorm.rdb.operator.builder.fragments.update;

import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.operator.builder.MetadataHelper;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.PrepareSqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.SqlFragments;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.function.FunctionFragmentBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateColumn;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperatorParameter;
import org.junit.Before;
import org.junit.Test;

import java.util.Collections;
import java.util.Map;

import static org.junit.Assert.*;

public class DefaultUpdateSqlBuilderTest {
    private DefaultUpdateSqlBuilder builder;

    @Before
    public void init() {
        RDBSchemaMetadata schema = MetadataHelper.createMockSchema();

        builder = DefaultUpdateSqlBuilder.of(schema.getTable("test").orElseThrow(NullPointerException::new));
    }


    @Test
    public void test() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();

        {
            UpdateColumn column = new UpdateColumn();
            column.setColumn("name");
            column.setValue("admin");
            parameter.getColumns().add(column);
        }

        {
            parameter.getWhere().addAll(Query.of().where("id", "1234").and().notNull("name").getParam().getTerms());
        }

        SqlRequest request = builder.build(parameter);
        System.out.println(request);
        assertEquals(request.getSql(), "update PUBLIC.test set \"NAME\" = ? where \"ID\" = ? and \"NAME\" is not null");
        assertArrayEquals(request.getParameters(), new Object[]{"admin", "1234"});

    }

    @Test
    public void testFunction() {
        UpdateOperatorParameter parameter = new UpdateOperatorParameter();


        {
            UpdateColumn column = new UpdateColumn();
            column.setFunction("incr");
            column.setOpts(Collections.singletonMap("incr", 1));
            column.setColumn("name");
            column.setValue("admin");
            parameter.getColumns().add(column);
        }

        {
            parameter.getWhere().addAll(Query.of().where("id", "1234").getParam().getTerms());
        }

        builder.getTable()
                .getColumn("name")
                .ifPresent(column -> column.addFeature(new FunctionFragmentBuilder() {
                    @Override
                    public String getFunction() {
                        return "incr";
                    }

                    @Override
                    public SqlFragments create(String columnFullName, RDBColumnMetadata metadata, Map<String, Object> opts) {
                        return PrepareSqlFragments.of().addSql(columnFullName, "+ ?")
                                .addParameter(opts.getOrDefault("incr", 1));
                    }

                    @Override
                    public String getName() {
                        return "自增";
                    }
                }));

        SqlRequest request = builder.build(parameter);
        System.out.println(request);
        assertEquals("update PUBLIC.test set \"NAME\" = name + ? where \"ID\" = ?",request.getSql());
        assertArrayEquals(request.getParameters(), new Object[]{1, "1234"});

    }
}