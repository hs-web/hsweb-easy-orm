package org.hswebframework.ezorm.rdb.operator;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.RecordReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.RecordSyncRepository;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ddl.DefaultTableBuilder;
import org.hswebframework.ezorm.rdb.operator.ddl.TableBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.ExecutableDeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.ExecutableInsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.ExecutableQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.ExecutableUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.DefaultUpsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;

@AllArgsConstructor(staticName = "of")
public class DefaultDatabaseOperator
        implements DatabaseOperator, DMLOperator, SQLOperator, DDLOperator {

    private final RDBDatabaseMetadata metadata;

    @Override
    public RDBDatabaseMetadata getMetadata() {
        return metadata;
    }

    @Override
    public DMLOperator dml() {
        return this;
    }

    @Override
    public DDLOperator ddl() {
        return this;
    }

    @Override
    public SQLOperator sql() {
        return this;
    }

    @Override
    public DeleteOperator delete(String table) {
        return ExecutableDeleteOperator.of(metadata
                .getTable(table)
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist ")));
    }

    @Override
    public UpsertOperator upsert(String table) {
        return DefaultUpsertOperator.of(metadata
                .getTable(table)
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist ")));
    }

    @Override
    public QueryOperator query(String tableOrView) {
        return new ExecutableQueryOperator(metadata
                .getTableOrView(tableOrView)
                .orElseThrow(() -> new UnsupportedOperationException("table or view [" + tableOrView + "] doesn't exist ")));
    }

    @Override
    public UpdateOperator update(String table) {

        return ExecutableUpdateOperator.of(metadata
                .getTable(table)
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist ")));
    }

    @Override
    public InsertOperator insert(String table) {
        return ExecutableInsertOperator.of(metadata
                .getTable(table)
                .orElseThrow(() -> new UnsupportedOperationException("table [" + table + "] doesn't exist ")));
    }

    @Override
    public SyncSqlExecutor sync() {
        return metadata.getFeature(SyncSqlExecutor.ID)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
    }

    @Override
    public ReactiveSqlExecutor reactive() {
        return metadata.getFeature(ReactiveSqlExecutor.ID)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported ReactiveSqlExecutor"));
    }

    @Override
    public TableBuilder createOrAlter(String name) {
        RDBTableMetadata table = metadata.getTable(name)
                .map(RDBTableMetadata::clone)
                .orElseGet(() -> {
                    String tableName = name;
                    RDBSchemaMetadata schema;
                    if (name.contains(".")) {
                        String[] arr = name.split("[.]");
                        tableName = arr[1];
                        schema = metadata.getSchema(arr[0]).orElseThrow(() -> new UnsupportedOperationException("schema [" + arr[0] + "] doesn't exist "));
                    } else {
                        schema = metadata.getCurrentSchema();
                    }

                    return schema.newTable(tableName);
                });

        return new DefaultTableBuilder(table);
    }

    @Override
    public TableBuilder createOrAlter(RDBTableMetadata newTable) {

        return new DefaultTableBuilder(newTable);
    }

    @Override
    public <K> ReactiveRepository<Record, K> createReactiveRepository(String tableName) {
        return  new RecordReactiveRepository<>(this, tableName);
    }

    @Override
    public <K> SyncRepository<Record, K> createRepository(String tableName) {
        return new RecordSyncRepository<>(this,tableName);
    }
}
