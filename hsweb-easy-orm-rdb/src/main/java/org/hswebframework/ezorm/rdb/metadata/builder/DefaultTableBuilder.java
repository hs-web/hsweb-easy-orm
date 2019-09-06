package org.hswebframework.ezorm.rdb.metadata.builder;

import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;

import java.util.Set;
import java.util.function.Consumer;

/**
 * @author zhouhao
 */
public class DefaultTableBuilder implements TableBuilder {
    private RDBTableMetadata table;

    private RDBSchemaMetadata schema;

    public DefaultTableBuilder(RDBTableMetadata table) {
        this.table = table;
        this.schema = table.getSchema();
    }

    @Override
    public IndexBuilder index() {
        return new IndexBuilder(this, table);
    }

    public TableBuilder custom(Consumer<RDBTableMetadata> consumer) {
        consumer.accept(table);
        return this;
    }

    @Override
    public TableBuilder addColumn(Set<RDBColumnMetadata> columns) {
        columns.forEach(table::addColumn);
        return this;
    }

    @Override
    public ColumnBuilder addOrAlterColumn(String name) {
        RDBColumnMetadata rdbColumnMetaData = table.getColumn(name)
                .orElseGet(() -> {
                    RDBColumnMetadata columnMetaData = new RDBColumnMetadata();
                    columnMetaData.setName(name);
                    return columnMetaData;
                });

        return new DefaultColumnBuilder(rdbColumnMetaData, this, table);
    }

    @Override
    public TableBuilder removeColumn(String name) {
        table.removeColumn(name);
        return this;
    }

    @Override
    public ColumnBuilder addColumn() {
        RDBColumnMetadata rdbColumnMetaData = new RDBColumnMetadata();
        return new DefaultColumnBuilder(rdbColumnMetaData, this, table);
    }

    @Override
    public TableBuilder comment(String comment) {
        table.setComment(comment);
        return this;
    }

    @Override
    public TableBuilder property(String propertyName, Object value) {
        // table.setProperty(propertyName, value);
        return this;
    }

    @Override
    public TableBuilder alias(String name) {
        table.setAlias(name);
        return this;
    }

    @Override
    public void commit() {
        SyncSqlExecutor executor = schema.<SyncSqlExecutor>findFeature(SyncSqlExecutor.id)
                .orElseThrow(() -> new UnsupportedOperationException("Unsupported SyncSqlExecutor"));
        RDBTableMetadata oldTable = schema.getTable(table.getName()).orElse(null);

        //alter
        if (oldTable != null) {
            schema.<AlterTableSqlBuilder>findFeature(AlterTableSqlBuilder.id)
                    .map(builder -> builder.build(AlterRequest.builder()
                            .allowDrop(false)
                            .newTable(table)
                            .oldTable(oldTable)
                            .build()))
                    .orElseThrow(() -> new UnsupportedOperationException("Unsupported AlterTableSqlBuilder"));
        } else {
            //create
            SqlRequest createTableSql = schema.<CreateTableSqlBuilder>findFeature(CreateTableSqlBuilder.id)
                    .map(builder -> builder.build(table))
                    .orElseThrow(() -> new UnsupportedOperationException("Unsupported CreateTableSqlBuilder"));
            executor.execute(createTableSql);
        }

        schema.addTable(table);

    }
}
