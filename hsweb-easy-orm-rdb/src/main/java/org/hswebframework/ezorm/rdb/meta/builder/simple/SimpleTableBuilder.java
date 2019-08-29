package org.hswebframework.ezorm.rdb.meta.builder.simple;

import org.hswebframework.ezorm.core.meta.ObjectMetaDataParser;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.meta.builder.ColumnBuilder;
import org.hswebframework.ezorm.rdb.meta.builder.IndexBuilder;
import org.hswebframework.ezorm.rdb.meta.builder.TableBuilder;

import java.sql.SQLException;
import java.util.Set;
import java.util.function.Consumer;

/**
 * @author zhouhao
 */
public class SimpleTableBuilder implements TableBuilder {
    public RDBTableMetadata rdbTableMetaData;
    public ObjectMetaDataParser parser;

    public SimpleTableBuilder(RDBTableMetadata rdbTableMetaData, ObjectMetaDataParser parser) {
        this.rdbTableMetaData = rdbTableMetaData;
        this.parser = parser;
    }

    @Override
    public IndexBuilder index() {
        return new IndexBuilder(this, rdbTableMetaData);
    }

    public TableBuilder custom(Consumer<RDBTableMetadata> consumer) {
        consumer.accept(rdbTableMetaData);
        return this;
    }

    @Override
    public TableBuilder addColumn(Set<RDBColumnMetadata> columns) {
        columns.forEach(rdbTableMetaData::addColumn);
        return this;
    }

    @Override
    public ColumnBuilder addOrAlterColumn(String name) {
        RDBColumnMetadata rdbColumnMetaData = rdbTableMetaData.getColumn(name)
                .orElseGet(() -> {
                    RDBColumnMetadata columnMetaData = new RDBColumnMetadata();
                    columnMetaData.setName(name);
                    return columnMetaData;
                });

        return new SimpleColumnBuilder(rdbColumnMetaData, this, rdbTableMetaData);
    }

    @Override
    public TableBuilder removeColumn(String name) {
        rdbTableMetaData.removeColumn(name);
        return this;
    }

    @Override
    public ColumnBuilder addColumn() {
        RDBColumnMetadata rdbColumnMetaData = new RDBColumnMetadata();
        return new SimpleColumnBuilder(rdbColumnMetaData, this, rdbTableMetaData);
    }

    @Override
    public TableBuilder comment(String comment) {
        rdbTableMetaData.setComment(comment);
        return this;
    }

    @Override
    public TableBuilder property(String propertyName, Object value) {
        // rdbTableMetaData.setProperty(propertyName, value);
        return this;
    }

    @Override
    public TableBuilder alias(String name) {
        rdbTableMetaData.setAlias(name);
        return this;
    }

    @Override
    public void commit() throws SQLException {
        // TODO: 2019-08-29
    }
}
