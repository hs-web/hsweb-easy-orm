package org.hswebframework.ezorm.rdb.mapping.events;

import lombok.SneakyThrows;
import org.hswebframework.ezorm.rdb.events.ContextKey;
import org.hswebframework.ezorm.rdb.events.ContextKeyValue;
import org.hswebframework.ezorm.rdb.executor.wrapper.ColumnWrapperContext;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

@SuppressWarnings("all")
public interface MappingContextKeys {

    ContextKey result = ContextKey.of("result");
    ContextKey instance = ContextKey.of("instance");
    ContextKey context = ContextKey.of("context");
    ContextKey type = ContextKey.of("type");
    ContextKey executorType = ContextKey.of("executorType");

    ContextKey<DMLOperator> dml = ContextKey.of("dml");

    ContextKey<QueryOperator> query = ContextKey.of("query");
    ContextKey<UpdateOperator> update = ContextKey.of("update");
    ContextKey<DeleteOperator> delete = ContextKey.of("delete");
    ContextKey<InsertOperator> insert = ContextKey.of("insert");

    static ContextKeyValue<String> type(String val) {
        return ContextKeyValue.of(type, val);
    }

    static ContextKeyValue<String> executorType(String val) {
        return ContextKeyValue.of(executorType, val);
    }

    static <T> ContextKeyValue<DMLOperator> dml(DMLOperator operator) {
        return ContextKeyValue.of(dml, operator);
    }

    static <T> ContextKeyValue<QueryOperator> query(QueryOperator operator) {
        return ContextKeyValue.of(query, operator);
    }

    static <T> ContextKeyValue<DeleteOperator> delete(DeleteOperator operator) {
        return ContextKeyValue.of(delete, operator);
    }

    static <T> ContextKeyValue<UpdateOperator> update(UpdateOperator operator) {
        return ContextKeyValue.of(update, operator);
    }

    static <T> ContextKeyValue<InsertOperator> insert(InsertOperator operator) {
        return ContextKeyValue.of(insert, operator);
    }

    static <T> ContextKeyValue<ColumnWrapperContext<T>> columnWrapperContext(ColumnWrapperContext<T> val) {
        return ContextKeyValue.of(context, val);
    }

    static <T> ContextKeyValue<T> instance(T val) {
        return ContextKeyValue.of(instance, val);
    }

    static <T> ContextKeyValue<T> result(T val) {
        return ContextKeyValue.of(result, val);
    }


}
