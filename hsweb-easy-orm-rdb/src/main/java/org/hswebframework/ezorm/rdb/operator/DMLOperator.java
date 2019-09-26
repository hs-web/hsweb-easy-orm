package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

public interface DMLOperator {

    QueryOperator query(String tableOrView);

    UpdateOperator update(String table);

    InsertOperator insert(String table);

    DeleteOperator delete(String table);

    <K> SyncRepository<Record, K> createRepository(String tableName);

    <K> ReactiveRepository<Record, K> createReactiveRepository(String tableName);

}
