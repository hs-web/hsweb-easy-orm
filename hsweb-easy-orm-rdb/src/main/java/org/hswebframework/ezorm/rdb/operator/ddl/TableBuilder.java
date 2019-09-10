package org.hswebframework.ezorm.rdb.operator.ddl;

import org.hswebframework.ezorm.rdb.metadata.RDBColumnMetadata;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;

import java.util.Set;
import java.util.function.Consumer;

public interface TableBuilder {

    TableBuilder addColumn(Set<RDBColumnMetadata> columns);

    TableBuilder custom(Consumer<RDBTableMetadata> consumer);

    ColumnBuilder addColumn();

    ColumnBuilder addOrAlterColumn(String name);

    TableBuilder removeColumn(String name);

    TableBuilder dropColumn(String name);

    TableBuilder comment(String comment);

    TableBuilder property(String propertyName, Object value);

    TableBuilder alias(String name);

    IndexBuilder index();

    ForeignKeyDSLBuilder  foreignKey();

    void commit();
}
