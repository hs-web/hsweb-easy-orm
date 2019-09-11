package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.executor.AsyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.executor.SyncSqlExecutor;
import org.hswebframework.ezorm.rdb.executor.reactive.ReactiveSqlExecutor;
import org.hswebframework.ezorm.rdb.metadata.*;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterRequest;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.AlterTableSqlBuilder;
import org.hswebframework.ezorm.rdb.operator.builder.fragments.ddl.CreateTableSqlBuilder;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletionStage;
import java.util.function.Consumer;

/**
 * @author zhouhao
 */
public class DefaultTableBuilder implements TableBuilder {
    private RDBTableMetadata table;

    private RDBSchemaMetadata schema;

    private boolean dropColumn = false;

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

    public TableBuilder custom(Consumer<RDBTableMetadata> consumer) {
        consumer.accept(table);
        return this;
    }

    @Override
    public TableBuilder addColumn(RDBColumnMetadata column) {
        table.addColumn(column);
        return this;
    }

    @Override
    public ColumnBuilder addColumn(String name) {
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
    public TableBuilder dropColumn(String name) {
        table.removeColumn(name);
        dropColumn = true;
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
    public TableBuilder alias(String name) {
        table.setAlias(name);
        return this;
    }

    @Override
    public TableDDLResultOperator commit() {
        RDBTableMetadata oldTable = schema.getTable(table.getName()).orElse(null);
        SqlRequest sqlRequest;
        //alter
        if (oldTable != null) {
            sqlRequest = schema.<AlterTableSqlBuilder>findFeature(AlterTableSqlBuilder.id)
                    .map(builder -> builder.build(AlterRequest.builder()
                            .allowDrop(dropColumn)
                            .newTable(table)
                            .oldTable(oldTable)
                            .build()))
                    .orElseThrow(() -> new UnsupportedOperationException("Unsupported AlterTableSqlBuilder"));

        } else {
            //create
            sqlRequest = schema.<CreateTableSqlBuilder>findFeature(CreateTableSqlBuilder.id)
                    .map(builder -> builder.build(table))
                    .orElseThrow(() -> new UnsupportedOperationException("Unsupported CreateTableSqlBuilder"));
        }
        return TableDDLResultOperator.of(schema, sqlRequest, () -> schema.addTable(table));


    }
}
