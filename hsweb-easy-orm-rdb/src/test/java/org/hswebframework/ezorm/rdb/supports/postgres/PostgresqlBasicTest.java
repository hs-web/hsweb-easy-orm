package org.hswebframework.ezorm.rdb.supports.postgres;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.supports.BasicCommonTests;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.sql.JDBCType;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

public class PostgresqlBasicTest extends BasicCommonTests {
    @Override
    protected RDBSchemaMetadata getSchema() {
        return new PostgresqlSchemaMetadata("public");
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.POSTGRES;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new PostgresqlConnectionProvider());
    }

    String jsonbTableName = "test_jsonb_exist_sync";

    @Test
    public void testJsonbExistTerm() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        try {

            operator.sql()
                    .sync()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "1",
                                            "{\"name\":\"JetLinks\",\"age\":18}"));
            operator.sql()
                    .sync()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "2",
                                            "{\"name\":\"Other\"}"));
            operator.sql()
                    .sync()
                    .execute(SqlRequests.of("insert into public." + jsonbTableName + " (id,data) values (?, ?::jsonb)",
                                            "3",
                                            "{\"age\":20,\"status\":\"ok\"}"));

            List<String> existsIds = operator.dml()
                                             .query(jsonbTableName)
                                             .select("id")
                                             .where(q -> q.where("data$exist", "name"))
                                             .fetch(ResultWrappers.mapStream())
                                             .sync()
                                             .map(map -> String.valueOf(map.get("id")))
                                             .collect(Collectors.toList());

            List<String> anyIds = operator.dml()
                                          .query(jsonbTableName)
                                          .select("id")
                                          .where(q -> q.where("data$exist$any", Arrays.asList("name", "status")))
                                          .fetch(ResultWrappers.mapStream())
                                          .sync()
                                          .map(map -> String.valueOf(map.get("id")))
                                          .collect(Collectors.toList());

            List<String> allIds = operator.dml()
                                          .query(jsonbTableName)
                                          .select("id")
                                          .where(q -> q.where("data$exist$all", Arrays.asList("name", "age")))
                                          .fetch(ResultWrappers.mapStream())
                                          .sync()
                                          .map(map -> String.valueOf(map.get("id")))
                                          .collect(Collectors.toList());

            List<String> containsIds = operator.dml()
                                               .query(jsonbTableName)
                                               .select("id")
                                               .where(q -> q.where("data$exist$contains", Collections.singletonMap("name", "JetLinks")))
                                               .fetch(ResultWrappers.mapStream())
                                               .sync()
                                               .map(map -> String.valueOf(map.get("id")))
                                               .collect(Collectors.toList());

            LinkedHashMap<String, Object> containedTarget = new LinkedHashMap<>();
            containedTarget.put("name", "JetLinks");
            containedTarget.put("age", 18);
            containedTarget.put("status", "ok");

            List<String> containedIds = operator.dml()
                                                .query(jsonbTableName)
                                                .select("id")
                                                .where(q -> q.where("data$exist$contained", containedTarget))
                                                .fetch(ResultWrappers.mapStream())
                                                .sync()
                                                .map(map -> String.valueOf(map.get("id")))
                                                .collect(Collectors.toList());

            List<String> commonContainsIds = operator.dml()
                                                    .query(jsonbTableName)
                                                    .select("id")
                                                    .where(q -> q.where("data$contains", Collections.singletonMap("name", "JetLinks")))
                                                    .fetch(ResultWrappers.mapStream())
                                                    .sync()
                                                    .map(map -> String.valueOf(map.get("id")))
                                                    .collect(Collectors.toList());

            List<String> commonContainedIds = operator.dml()
                                                     .query(jsonbTableName)
                                                     .select("id")
                                                     .where(q -> q.where("data$contained", containedTarget))
                                                     .fetch(ResultWrappers.mapStream())
                                                     .sync()
                                                     .map(map -> String.valueOf(map.get("id")))
                                                     .collect(Collectors.toList());

            List<String> commonAllKeyIds = operator.dml()
                                                  .query(jsonbTableName)
                                                  .select("id")
                                                  .where(q -> q.where("data$contains$all", Arrays.asList("name", "age")))
                                                  .fetch(ResultWrappers.mapStream())
                                                  .sync()
                                                  .map(map -> String.valueOf(map.get("id")))
                                                  .collect(Collectors.toList());

            List<String> commonOverlapIds = operator.dml()
                                                  .query(jsonbTableName)
                                                  .select("id")
                                                  .where(q -> q.where("data$overlap", Arrays.asList("name", "status")))
                                                  .fetch(ResultWrappers.mapStream())
                                                  .sync()
                                                  .map(map -> String.valueOf(map.get("id")))
                                                  .collect(Collectors.toList());

            List<String> commonNotContainsIds = operator.dml()
                                                      .query(jsonbTableName)
                                                      .select("id")
                                                      .where(q -> q.where("data$ncontains", Collections.singletonMap("name", "JetLinks")))
                                                      .fetch(ResultWrappers.mapStream())
                                                      .sync()
                                                      .map(map -> String.valueOf(map.get("id")))
                                                      .collect(Collectors.toList());

            Assert.assertEquals(Arrays.asList("1", "2"), existsIds);
            Assert.assertEquals(Arrays.asList("1", "2", "3"), anyIds);
            Assert.assertEquals(List.of("1"), allIds);
            Assert.assertEquals(List.of("1"), containsIds);
            Assert.assertEquals(List.of("1"), containedIds);
            Assert.assertEquals(List.of("1"), commonContainsIds);
            Assert.assertEquals(List.of("1"), commonContainedIds);
            Assert.assertEquals(List.of("1"), commonAllKeyIds);
            Assert.assertEquals(Arrays.asList("1", "2", "3"), commonOverlapIds);
            Assert.assertEquals(Arrays.asList("2", "3"), commonNotContainsIds);
        } finally {
            try {
                operator.sql()
                        .sync()
                        .execute(SqlRequests.of("drop table if exists public." + jsonbTableName));
            } catch (Exception ignore) {
            }
        }
    }

    @Before
    public void createJsonbTable() {
        RDBDatabaseMetadata database = getDatabase();
        DatabaseOperator operator = DefaultDatabaseOperator.of(database);

        operator.ddl()
                .createOrAlter(jsonbTableName)
                .addColumn().name("id").varchar(32).primaryKey().comment("ID").commit()
                .addColumn().name("data").type(JsonbType.INSTANCE).comment("数据").commit()
                .commit()
                .sync();
    }
}
