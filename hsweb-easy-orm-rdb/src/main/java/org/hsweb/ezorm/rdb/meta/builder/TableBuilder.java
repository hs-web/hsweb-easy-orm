package org.hsweb.ezorm.rdb.meta.builder;

import org.hsweb.ezorm.rdb.meta.RDBColumnMetaData;

import java.sql.SQLException;
import java.util.Set;

public interface TableBuilder {

    TableBuilder addColumn(Set<RDBColumnMetaData> columns);

    ColumnBuilder addColumn();

    TableBuilder comment(String comment);

    void commit() throws SQLException;
}
