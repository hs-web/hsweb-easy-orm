package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.core.param.TermType;
import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.junit.Assert;
import org.junit.Test;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AbstractJsonQueryIntegrationTest {

    protected abstract RDBSchemaMetadata getSchema();

    protected abstract Dialect getDialect();

    protected abstract SyncSqlExecutor getSqlExecutor();

    protected abstract DataType getJsonType();

    protected boolean supportContained() {
        return false;
    }

    @Test
    public void testJsonQueryTerms() {
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(getDialect());
        RDBSchemaMetadata schema = getSchema();
        database.setCurrentSchema(schema);
        database.addSchema(schema);
        database.addFeature(getSqlExecutor());
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        String tableName = "test_json_query";

        try {
            operator.ddl()
                    .createOrAlter(tableName)
                    .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                    .addColumn().name("data").type(getJsonType()).comment("JSON数据").commit()
                    .commit()
                    .sync();

            operator.dml()
                    .insert(tableName)
                    .columns("id", "data")
                    .values("1", json("JetLinks", 18, "ok"))
                    .values("2", Collections.singletonMap("name", "Other"))
                    .values("3", json(null, 20, "ok"))
                    .execute()
                    .sync();

            Assert.assertEquals(
                Arrays.asList("1", "2"),
                queryIds(operator, tableName, "data", JsonTermType.exists, "name")
            );
            Assert.assertEquals(
                Collections.singletonList("1"),
                queryIds(operator, tableName, "data", JsonTermType.contains, Collections.singletonMap("name", "JetLinks"))
            );
            Assert.assertEquals(
                Collections.singletonList("3"),
                queryIds(operator, tableName, "data", JsonTermType.value, JsonValueCondition.of("age", TermType.gt, 19))
            );
            Assert.assertEquals(
                Collections.singletonList("1"),
                queryIds(operator, tableName, "data", JsonTermType.value, JsonValueCondition.of("name", "JetLinks"))
            );

            if (supportContained()) {
                Map<String, Object> target = json("JetLinks", 18, "ok");
                target.put("extra", true);
                Assert.assertEquals(
                    Collections.singletonList("1"),
                    queryIds(operator, tableName, "data", JsonTermType.contained, target)
                );
            }
        } finally {
            try {
                operator.sql().sync().execute(SqlRequests.of("drop table " + tableName));
            } catch (Exception ignore) {
            }
        }
    }

    private List<String> queryIds(DatabaseOperator operator,
                                  String tableName,
                                  String column,
                                  String termType,
                                  Object value) {
        return operator.dml()
                       .query(tableName)
                       .select("id")
                       .where(q -> q.and(column, termType, value))
                       .orderBy(SortOrder.asc("id"))
                       .fetch(ResultWrappers.mapStream())
                       .sync()
                       .map(map -> String.valueOf(map.get("id")))
                       .collect(Collectors.toList());
    }

    private Map<String, Object> json(String name, int age, String status) {
        Map<String, Object> data = new LinkedHashMap<>();
        if (name != null) {
            data.put("name", name);
        }
        data.put("age", age);
        data.put("status", status);
        return data;
    }
}
