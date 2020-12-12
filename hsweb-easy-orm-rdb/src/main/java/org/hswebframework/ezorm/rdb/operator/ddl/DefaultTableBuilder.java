package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBSchemaMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author zhouhao
 */
public class DefaultTableBuilder implements TableBuilder {
    private final RDBTableMetadata table;

    private final RDBSchemaMetadata schema;

    private boolean dropColumn = false;
    private boolean allowAlter = true;
    private boolean autoLoad = true;

    public DefaultTableBuilder(RDBTableMetadata table) {
        this.table = table;
        this.schema = table.getSchema();
    }

    @Override
    public IndexBuilder index() {
        return new IndexBuilder(this, table);
    }

    @Override
    public ForeignKeyDSLBuilder foreignKey() {
        return new ForeignKeyDSLBuilder(table);
    }

    public DefaultTableBuilder custom(Consumer<RDBTableMetadata> consumer) {
        consumer.accept(table);
        return this;
    }

    @Override
    public DefaultTableBuilder addColumn(RDBColumnMetadata column) {
        table.addColumn(column);
        return this;
    }

    @Override
    public ColumnBuilder addColumn(String name) {
        RDBColumnMetadata rdbColumnMetaData = table.getColumn(name)
                                                   .orElseGet(() -> {
                                                       RDBColumnMetadata columnMetaData = table.newColumn();
                                                       columnMetaData.setName(name);
                                                       return columnMetaData;
                                                   });

        return new DefaultColumnBuilder(rdbColumnMetaData, this, table);
    }

    @Override
    public DefaultTableBuilder removeColumn(String name) {
        table.removeColumn(name);
        return this;
    }

    @Override
    public DefaultTableBuilder dropColumn(String name) {
        table.removeColumn(name);
        dropColumn = true;
        return this;
    }

    @Override
    public ColumnBuilder addColumn() {
        RDBColumnMetadata rdbColumnMetaData = table.newColumn();
        return new DefaultColumnBuilder(rdbColumnMetaData, this, table);
    }

    @Override
    public DefaultTableBuilder comment(String comment) {
        table.setComment(comment);
        return this;
    }

    @Override
    public DefaultTableBuilder alias(String name) {
        table.setAlias(name);
        return this;
    }

    @Override
    public DefaultTableBuilder allowAlter(boolean allow) {
        allowAlter = allow;
        return this;
    }

    @Override
    public TableBuilder autoLoad(boolean autoLoad) {
        this.autoLoad = autoLoad;
        return this;
    }

    private SqlRequest buildAlterSql(RDBTableMetadata oldTable) {
        return schema
                .findFeatureNow(AlterTableSqlBuilder.ID)
                .build(AlterRequest.builder()
                                   .allowDrop(dropColumn)
                                   .newTable(table)
                                   .allowAlter(allowAlter)
                                   .oldTable(oldTable)
                                   .build());
    }

    @Override
    public TableDDLResultOperator commit() {

        return new TableDDLResultOperator() {
            @Override
            public Boolean sync() {
                RDBTableMetadata oldTable = schema.getTable(table.getName(), autoLoad).orElse(null);
                SqlRequest sqlRequest;
                Runnable whenComplete;
                //alter
                if (oldTable != null) {
                    sqlRequest = buildAlterSql(oldTable);

                    whenComplete = () -> oldTable.merge(table);
                } else {
                    //create
                    sqlRequest = schema.findFeatureNow(CreateTableSqlBuilder.ID).build(table);
                    whenComplete = () -> schema.addTable(table);
                }
                if (sqlRequest.isEmpty()) {
                    whenComplete.run();
                    return true;
                }
                ExceptionUtils.translation(() -> {
                    schema.findFeatureNow(SyncSqlExecutor.ID).execute(sqlRequest);
                    return true;
                }, schema);
                whenComplete.run();
                return true;
            }

            @Override
            public Mono<Boolean> reactive() {

                ReactiveSqlExecutor sqlExecutor = schema.findFeatureNow(ReactiveSqlExecutor.ID);

                return schema
                        .getTableReactive(table.getName(), autoLoad)
                        .map(oldTable -> {
                            SqlRequest request = buildAlterSql(oldTable);
                            if (request.isEmpty()) {
                                oldTable.merge(table);
                                return Mono.just(true);
                            }
                            return sqlExecutor.execute(request)
                                              .doOnSuccess(ignore -> oldTable.merge(table))
                                              .thenReturn(true);
                        })
                        .switchIfEmpty(Mono.fromSupplier(() -> {
                            SqlRequest request = schema.findFeatureNow(CreateTableSqlBuilder.ID).build(table);
                            if (request.isEmpty()) {
                                return Mono.just(true);
                            }
                            return sqlExecutor.execute(request)
                                              .doOnSuccess(ignore -> schema.addTable(table))
                                              .thenReturn(true);
                        }))
                        .flatMap(Function.identity());
            }
        };


    }
}
