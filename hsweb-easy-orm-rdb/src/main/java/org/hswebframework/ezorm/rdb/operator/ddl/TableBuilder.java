package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.operator.ResultOperator;

import java.util.Set;
import java.util.function.Consumer;

public interface TableBuilder {

    TableBuilder addColumn(RDBColumnMetadata column);

    TableBuilder custom(Consumer<RDBTableMetadata> consumer);

    ColumnBuilder addColumn();

    ColumnBuilder addColumn(String name);

    TableBuilder removeColumn(String name);

    TableBuilder dropColumn(String name);

    TableBuilder comment(String comment);

    TableBuilder alias(String name);

    IndexBuilder index();

    ForeignKeyDSLBuilder  foreignKey();

    TableDDLResultOperator commit();
}
