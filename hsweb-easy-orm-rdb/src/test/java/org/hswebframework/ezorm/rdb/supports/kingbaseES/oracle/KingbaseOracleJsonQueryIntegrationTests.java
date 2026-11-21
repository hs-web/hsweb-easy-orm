package org.hswebframework.ezorm.rdb.supports.kingbaseES.oracle;

import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;
import org.hswebframework.ezorm.rdb.supports.json.AbstractJsonQueryIntegrationTest;
import org.hswebframework.ezorm.rdb.supports.json.JsonTermType;
import org.hswebframework.ezorm.rdb.supports.json.JsonType;
import org.hswebframework.ezorm.rdb.supports.json.JsonValueCondition;
import org.hswebframework.ezorm.rdb.supports.kingbaseES.KingbaseOfficialTestDatabase;
import org.hswebframework.ezorm.core.param.TermType;
import org.junit.Assert;
import org.junit.ClassRule;
import org.junit.Test;

import java.time.Duration;
import java.util.List;
import java.util.Map;

/** Runs the shared JSON query contract using native Oracle JSON conditions. */
public class KingbaseOracleJsonQueryIntegrationTests extends AbstractJsonQueryIntegrationTest {

    @ClassRule
    public static final KingbaseOfficialTestDatabase DATABASE = new KingbaseOfficialTestDatabase("oracle");

    @Test
    public void shouldExecuteNegatedAndNestedNativeOracleJsonConditions() {
        RDBSchemaMetadata schema = getSchema();
        RDBDatabaseMetadata database = new RDBDatabaseMetadata(getDialect());
        database.addSchema(schema);
        database.setCurrentSchema(schema);
        database.addFeature(getSqlExecutor());
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);
        String table = "test_oracle_json_nested";

        try {
            operator.ddl().createOrAlter(table)
                .addColumn("id").varchar(32).primaryKey().commit()
                .addColumn("data").type(getJsonType()).commit()
                .commit().sync();
            operator.dml().insert(table).columns("id", "data")
                .values("1", Map.of("name", "JetLinks", "profile", Map.of("age", 18, "city", "Beijing")))
                .values("2", Map.of("name", "Other", "profile", Map.of("age", 30, "city", "Shanghai")))
                .values("3", Map.of("name", "Third"))
                .execute().sync();

            Assert.assertEquals(List.of("1", "2"), queryIds(operator, table, JsonTermType.exists, "profile.age"));
            Assert.assertEquals(List.of("3"), queryIds(operator, table, JsonTermType.notExists, "profile.age"));
            Assert.assertEquals(List.of("2", "3"), queryIds(operator, table, JsonTermType.notContains,
                Map.of("name", "JetLinks")));
            Assert.assertEquals(List.of("1"), queryIds(operator, table, JsonTermType.contains,
                Map.of("profile", Map.of("city", "Beijing"))));
            Assert.assertEquals(List.of("2"), queryIds(operator, table, JsonTermType.value,
                JsonValueCondition.of("profile.age", TermType.gt, 20)));
        } finally {
            getSqlExecutor().execute(SqlRequests.of("drop table if exists " + schema.getQuoteName() + "." + table));
        }
    }

    private List<String> queryIds(DatabaseOperator operator, String table, String termType, Object value) {
        SqlRequest request = operator.dml().query(table)
            .context(Map.of("easyorm.strict.term", true))
            .select("id").where(terms -> terms.and("data", termType, value))
            .orderBy(SortOrder.asc("id")).getSql();
        System.out.println("[原生 Oracle JSON 条件] " + termType + ": " + request);
        return DATABASE.reactive().select(request, ResultWrappers.map())
            .map(row -> String.valueOf(row.get("id")))
            .collectList().block(Duration.ofSeconds(30));
    }

    @Override
    protected RDBSchemaMetadata getSchema() {
        return DATABASE.newSchema();
    }

    @Override
    protected Dialect getDialect() {
        return new KingbaseOracleDialect();
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return DATABASE.sync();
    }

    @Override
    protected DataType getJsonType() {
        return JsonType.CLOB;
    }
}
