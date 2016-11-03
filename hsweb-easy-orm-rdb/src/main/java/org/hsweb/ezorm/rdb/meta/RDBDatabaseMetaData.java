package org.hsweb.ezorm.rdb.meta;

import org.hsweb.ezorm.core.meta.AbstractDatabaseMetaData;
import org.hsweb.ezorm.core.meta.DatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.dialect.Dialect;
import org.hsweb.ezorm.rdb.render.SqlRender;

public abstract class RDBDatabaseMetaData extends AbstractDatabaseMetaData implements DatabaseMetaData {
    private TableMetaParser parser;

    public abstract Dialect getDialect();

    public abstract void init();

    public abstract SqlRender getRenderer(SqlRender.TYPE type);

    public abstract String getName();

    public void setParser(TableMetaParser parser) {
        this.parser = parser;
    }

    public TableMetaParser getParser() {
        return parser;
    }

    @Override
    public RDBTableMetaData getTableMetaData(String name) {
        return super.getTableMetaData(name);
    }

    public RDBTableMetaData removeTable(String name) {
        return tableMetaDataStorage.removeTableMeta(name);
    }

    public RDBTableMetaData putTable(RDBTableMetaData tableMetaData) {
        tableMetaData.setDatabaseMetaData(this);
        return tableMetaDataStorage.putTableMetaData(tableMetaData);
    }

    public void shutdown() {
        tableMetaDataStorage.clear();
    }

}
