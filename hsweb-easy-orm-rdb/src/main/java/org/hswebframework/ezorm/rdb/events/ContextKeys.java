package org.hswebframework.ezorm.rdb.events;

import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DatabaseOperator;

public interface ContextKeys {

    ContextKey<TableOrViewMetadata> table = ContextKey.of("table");

    ContextKey<?> source = ContextKey.of("source");

    ContextKey<DatabaseOperator> database = ContextKey.of("database");

    static <T> ContextKeyValue<T> source(T source) {
        return ContextKeys.<T>source().value(source);
    }

    static <T> ContextKey<T> source() {
        return (ContextKey) source;
    }

    static <T> ContextKeyValue<TableOrViewMetadata> tableMetadata(TableOrViewMetadata metadata) {
        return ContextKeyValue.of(table, metadata);
    }

}
