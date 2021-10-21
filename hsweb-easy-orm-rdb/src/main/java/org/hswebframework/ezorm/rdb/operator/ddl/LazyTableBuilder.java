package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.core.DefaultValue;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.hswebframework.ezorm.rdb.utils.ExceptionUtils;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

public class LazyTableBuilder implements TableBuilder {

    private final String tableName;
    private final RDBSchemaMetadata schema;
    private final Set<String> removed = new HashSet<>();

    private boolean dropColumn = false;
    private boolean allowAlter = true;
    private boolean autoLoad = true;
    private boolean merge = true;

    public LazyTableBuilder(RDBSchemaMetadata schema, String tableName) {
        this.tableName = tableName;
        this.schema = schema;
    }

    private final List<Consumer<RDBTableMetadata>> consumers = new ArrayList<>();

    @Override
    public TableBuilder addColumn(RDBColumnMetadata column) {
        consumers.add(table -> table.addColumn(column));
        return this;
    }

    @Override
    public TableBuilder custom(Consumer<RDBTableMetadata> consumer) {
        consumers.add(consumer);
        return this;
    }

    @Override
    public ColumnBuilder addColumn() {
        return new LazyColumnBuilder();
    }

    @Override
    public ColumnBuilder addColumn(String name) {
        return new LazyColumnBuilder().name(name);
    }

    @Override
    public TableBuilder removeColumn(String name) {
        removed.add(name);
        return this;
    }

    @Override
    public TableBuilder dropColumn(String name) {
        removed.add(name);
        dropColumn = true;
        return this;
    }

    @Override
    public TableBuilder comment(String comment) {
        return custom(table -> table.setComment(comment));
    }

    @Override
    public TableBuilder alias(String name) {
        return custom(table -> table.setAlias(name));
    }

    @Override
    public TableBuilder allowAlter(boolean allow) {
        this.allowAlter = allow;
        return this;
    }

    @Override
    public TableBuilder autoLoad(boolean autoLoad) {
        this.autoLoad = autoLoad;
        return this;
    }

    @Override
    public TableBuilder merge(boolean merge) {
        this.merge = merge;
        return this;
    }

    @Override
    public IndexBuilder index() {
        return new LazyIndexBuilder();
    }

    @Override
    public ForeignKeyDSLBuilder foreignKey() {
        return new LazyForeignKeyDSLBuilder();
    }

    private SqlRequest buildAlterSql(RDBTableMetadata table, RDBTableMetadata oldTable) {

        return schema
                .findFeatureNow(AlterTableSqlBuilder.ID)
                .build(AlterRequest.builder()
                                   .allowDrop(dropColumn)
                                   .newTable(table)
                                   .allowAlter(allowAlter)
                                   .oldTable(oldTable)
                                   .build());
    }

    private void accept(RDBTableMetadata table) {
        for (Consumer<RDBTableMetadata> consumer : consumers) {
            consumer.accept(table);
        }
    }

    @Override
    public TableDDLResultOperator commit() {

        return new TableDDLResultOperator() {
            @Override
            public Boolean sync() {
                RDBTableMetadata oldTable = schema.getTable(tableName, autoLoad).orElse(null);
                RDBTableMetadata newTable;
                if (oldTable != null) {
                    newTable = oldTable.clone();
                    removed.forEach(newTable::removeColumn);
                } else {
                    newTable = schema.newTable(tableName);
                }
                for (Consumer<RDBTableMetadata> consumer : consumers) {
                    consumer.accept(newTable);
                }
                SqlRequest sqlRequest;
                Runnable whenComplete;
                //alter
                if (oldTable != null) {
                    sqlRequest = buildAlterSql(newTable, oldTable);
                    if (merge) {
                        whenComplete = () -> {
                            oldTable.merge(newTable);
                            removed.forEach(oldTable::removeColumn);
                        };
                    } else {
                        whenComplete = () -> oldTable.replace(newTable);
                    }
                } else {
                    //create
                    sqlRequest = schema.findFeatureNow(CreateTableSqlBuilder.ID).build(newTable);
                    whenComplete = () -> schema.addTable(newTable);
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
                        .getTableReactive(tableName, autoLoad)
                        .map(oldTable -> {
                            RDBTableMetadata newTable = oldTable.clone();
                            accept(newTable);
                            removed.forEach(newTable::removeColumn);
                            SqlRequest request = buildAlterSql(newTable, oldTable);
                            if (request.isEmpty()) {
                                if (merge) {
                                    oldTable.merge(newTable);
                                    removed.forEach(oldTable::removeColumn);
                                } else {
                                    oldTable.replace(newTable);
                                }
                                return Mono.just(true);
                            }
                            return sqlExecutor.execute(request)
                                              .doOnSuccess(ignore -> {
                                                  oldTable.merge(newTable);
                                                  removed.forEach(oldTable::removeColumn);
                                              })
                                              .thenReturn(true);
                        })
                        .switchIfEmpty(Mono.fromSupplier(() -> {
                            RDBTableMetadata newTable = schema.newTable(tableName);
                            accept(newTable);
                            SqlRequest request = schema.findFeatureNow(CreateTableSqlBuilder.ID).build(newTable);
                            if (request.isEmpty()) {
                                return Mono.just(true);
                            }
                            return sqlExecutor
                                    .execute(request)
                                    .doOnSuccess(ignore -> schema.addTable(newTable))
                                    .thenReturn(true);
                        }))
                        .flatMap(Function.identity());
            }
        };
    }

    class LazyColumnBuilder implements ColumnBuilder {

        private final List<Consumer<RDBColumnMetadata>> operations = new ArrayList<>();

        @Override
        public ColumnBuilder custom(Consumer<RDBColumnMetadata> consumer) {
            operations.add(consumer);
            return this;
        }

        @Override
        public ColumnBuilder name(String name) {
            operations.add(column -> column.setName(name));
            return this;
        }

        @Override
        public ColumnBuilder alias(String name) {
            operations.add(column -> column.setAlias(name));
            return this;
        }

        @Override
        public ColumnBuilder dataType(String dataType) {
            operations.add(column -> column.setDataType(dataType));
            return this;
        }

        @Override
        public ColumnBuilder type(String typeId) {

            operations.add(column -> column.setType(column.getDialect().convertDataType(typeId)));
            return this;
        }

        @Override
        public ColumnBuilder type(DataType type) {
            operations.add(column -> column.setType(type));
            return this;
        }

        @Override
        public ColumnBuilder comment(String comment) {
            operations.add(column -> column.setComment(comment));
            return this;
        }

        @Override
        public ColumnBuilder notNull() {
            operations.add(column -> column.setNotNull(true));
            return this;
        }

        @Override
        public ColumnBuilder primaryKey() {
            operations.add(column -> column.setPrimaryKey(true));
            return this;
        }

        @Override
        public ColumnBuilder columnDef(String def) {
            operations.add(column -> column.setColumnDefinition(def));
            return this;
        }

        @Override
        public ColumnBuilder defaultValue(DefaultValue value) {
            operations.add(column -> column.setDefaultValue(value));
            return this;
        }

        @Override
        public ColumnBuilder property(String propertyName, Object value) {
            operations.add(column -> column.setProperty(propertyName, value));
            return this;
        }

        @Override
        public ColumnBuilder length(int len) {
            operations.add(column -> column.setLength(len));
            return this;
        }

        @Override
        public ColumnBuilder length(int precision, int scale) {
            operations.add(column -> {
                column.setLength(precision);
                column.setScale(scale);
                column.setPrecision(precision);
            });
            return this;
        }

        @Override
        public TableBuilder commit() {
            LazyTableBuilder.this.custom(table -> {
                RDBColumnMetadata column = table.newColumn();
                for (Consumer<RDBColumnMetadata> operation : operations) {
                    operation.accept(column);
                }
                if (column.getValueCodec() == null) {
                    table.findFeature(ValueCodecFactory.ID)
                         .flatMap(factory -> factory.createValueCodec(column))
                         .ifPresent(column::setValueCodec);
                }
                table.addColumn(column);
            });
            return LazyTableBuilder.this;
        }
    }

    class LazyIndexBuilder extends IndexBuilder {

        public LazyIndexBuilder() {
            super(LazyTableBuilder.this, null);
        }

        @Override
        public TableBuilder commit() {
            LazyTableBuilder.this.custom(table -> table.addIndex(index));
            return LazyTableBuilder.this;
        }
    }

    class LazyForeignKeyDSLBuilder extends ForeignKeyDSLBuilder {

        public LazyForeignKeyDSLBuilder() {
            super(LazyTableBuilder.this, null);
        }

        @Override
        public TableBuilder commit() {
            LazyTableBuilder.this.custom(table -> table.addForeignKey(builder));
            return LazyTableBuilder.this;
        }
    }
}
