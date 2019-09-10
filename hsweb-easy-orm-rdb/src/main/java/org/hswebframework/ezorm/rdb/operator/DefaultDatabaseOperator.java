package org.hswebframework.ezorm.rdb.operator;

import lombok.AllArgsConstructor;
import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ddl.DefaultTableBuilder;
import org.hswebframework.ezorm.rdb.operator.ddl.TableBuilder;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.ExecutableDeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.ExecutableInsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.query.ExecutableQueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.ExecutableUpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

@AllArgsConstructor(staticName = "of")
public class DefaultDatabaseOperator
        implements DatabaseOperator, DMLOperator, SQLOperator, DDLOperator {

    private RDBDatabaseMetadata metadata;

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
    public QueryOperator query() {
        return new ExecutableQueryOperator(metadata);
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
        return metadata.<SyncSqlExecutor>getFeature(SyncSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported SyncSqlExecutor"));
    }

    @Override
    public AsyncSqlExecutor async() {
        return metadata.<AsyncSqlExecutor>getFeature(AsyncSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("unsupported AsyncSqlExecutor"));
    }

    @Override
    public ReactiveSqlExecutor reactive() {
        return metadata.<ReactiveSqlExecutor>getFeature(ReactiveSqlExecutor.id)
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
                    }else {
                        schema = metadata.getCurrentSchema();
                    }
                    RDBTableMetadata newTable = new RDBTableMetadata(tableName);
                    newTable.setSchema(schema);
                    return newTable;
                });

        return new DefaultTableBuilder(table);
    }
}
