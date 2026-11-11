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
import org.hswebframework.ezorm.rdb.supports.mysql.Mysql8ConnectionProvider;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSaveOrUpdateOperator;
import org.hswebframework.ezorm.rdb.supports.mysql.MysqlSchemaMetadata;
import org.junit.Test;

import java.util.Arrays;
import java.util.UUID;

import static org.junit.Assert.*;

public class Mysql8JsonQueryIntegrationTest extends AbstractJsonQueryIntegrationTest {

    @Test
    public void legacyUpsertClearsJsonWithExplicitNull() {
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
            assertNotNull(database.dml().query(table).fetch(ResultWrappers.singleMap()).sync().get("data"));
            UpsertOperatorParameter parameter = new UpsertOperatorParameter();
            for (String columnName : Arrays.asList("id", "data")) {
                UpsertColumn column = new UpsertColumn();
                column.setColumn(columnName);
                parameter.getColumns().add(column);
            }
            parameter.getValues().add(Arrays.asList("one", NullValue.of(getJsonType())));
            new MysqlSaveOrUpdateOperator(table).execute(parameter).sync();
            assertNull(database.dml().query(table).fetch(ResultWrappers.singleMap()).sync().get("data"));
        } finally {
            database.sql().sync().execute(SqlRequests.of("drop table if exists " + name));
        }
    }

    @Override
    protected RDBSchemaMetadata getSchema() {
        return new MysqlSchemaMetadata(System.getProperty("mysql8.db", "ezorm"));
    }

    @Override
    protected Dialect getDialect() {
        return Dialect.MYSQL;
    }

    @Override
    protected SyncSqlExecutor getSqlExecutor() {
        return new TestSyncSqlExecutor(new Mysql8ConnectionProvider());
    }

    @Override
    protected DataType getJsonType() {
        return JsonType.INSTANCE;
    }
}
