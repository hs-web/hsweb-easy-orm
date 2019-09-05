package org.hswebframework.ezorm.rdb.metadata.builder;

import org.hswebframework.ezorm.rdb.metadata.RDBIndexMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

/**
 * @author zhouhao
 * @since 1.0.0
 */
public class IndexBuilder {
    private TableBuilder tableBuilder;

    public IndexBuilder(TableBuilder tableBuilder, RDBTableMetadata table) {
        this.tableBuilder = tableBuilder;
        this.table = table;
    }

    private RDBTableMetadata table;

    private RDBIndexMetadata index = new RDBIndexMetadata();

    public IndexBuilder name(String indexName) {
        index.setIndexName(indexName);
        return this;
    }
    public IndexBuilder unique() {
        index.setUnique(true);
        return this;
    }
    public IndexBuilder column(String column) {
        return column(column, null);
    }

    public IndexBuilder column(String column, String sort) {
        index.getColumnName().add(RDBIndexMetadata.IndexColumn.of(column, sort));
        return this;
    }

    public TableBuilder commit() {
        table.getIndexes().add(index);
        return tableBuilder;
    }

}
