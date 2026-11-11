package org.hswebframework.ezorm.rdb.supports.json;

import org.hswebframework.ezorm.rdb.TestSyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.NullValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequests;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.DataType;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.dialect.Dialect;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.DefaultDatabaseOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertColumn;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperatorParameter;
import org.hswebframework.ezorm.rdb.supports.postgres.JsonbType;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.supports.postgres.PostgresqlSchemaMetadata;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.UUID;

import static org.junit.Assert.*;

public class PostgresqlJsonQueryIntegrationTest extends AbstractJsonQueryIntegrationTest {

    @Test
    public void legacyUpsertUpdatesAndClearsJson() {
        RDBDatabaseMetadata metadata = new RDBDatabaseMetadata(getDialect());
        RDBSchemaMetadata schema = getSchema();
        metadata.addSchema(schema);
        metadata.setCurrentSchema(schema);
        metadata.addFeature(getSqlExecutor());
        DatabaseOperator database = DefaultDatabaseOperator.of(metadata);
        String name = "json_legacy_" + UUID.randomUUID().toString().replace("-", "");
        try {
            database.ddl().createOrAlter(name)
                .addColumn().name("id").varchar(32).primaryKey().commit()
                .addColumn().name("data").type(getJsonType()).commit().commit().sync();
            RDBTableMetadata table = metadata.getTable(name).orElseThrow(AssertionError::new);
            database.dml().insert(table).columns("id", "data").values("one", "{}").execute().sync();
            UpsertOperatorParameter parameter = new UpsertOperatorParameter();
            for (String columnName : Arrays.asList("id", "data")) {
                UpsertColumn column = new UpsertColumn();
                column.setColumn(columnName);
                parameter.getColumns().add(column);
            }
            parameter.getValues().add(Arrays.asList("one", Collections.singletonMap("name", "updated")));
            PostgresqlSaveOrUpdateOperator operator = new PostgresqlSaveOrUpdateOperator(table);
            operator.execute(parameter).sync();
            assertTrue(String.valueOf(database.dml().query(table).fetch(ResultWrappers.singleMap()).sync().get("data"))
                .contains("updated"));
            parameter.getValues().set(0, Arrays.asList("one", NullValue.of(getJsonType())));
            operator.execute(parameter).sync();
            assertNull(database.dml().query(table).fetch(ResultWrappers.singleMap()).sync().get("data"));
        } finally {
            database.sql().sync().execute(SqlRequests.of("drop table if exists " + name));
        }
    }

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

    @Override
    protected DataType getJsonType() {
        return JsonbType.INSTANCE;
    }

    @Override
    protected boolean supportContained() {
        return true;
    }
}
