package org.hsweb.ezorm.rdb.meta.builder;

import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;

import java.sql.SQLException;
import java.util.Set;

public interface TableBuilder {

    TableBuilder addColumn(Set<RDBColumnMetaData> columns);

    ColumnBuilder addColumn();

    TableBuilder comment(String comment);

    TableBuilder property(String propertyName, Object value);
    TableBuilder alias(String name);

    void commit() throws SQLException;
}
