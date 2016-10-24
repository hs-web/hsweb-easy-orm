package org.hsweb.ezorm.meta.builder;

import org.hsweb.ezorm.meta.ColumnMetaData;

import java.sql.SQLException;
import java.util.Set;

public interface TableBuilder {

    TableBuilder addColumn(Set<ColumnMetaData> columns);

    ColumnBuilder addColumn();

    TableBuilder comment(String comment);

    void commit() throws SQLException;

}
