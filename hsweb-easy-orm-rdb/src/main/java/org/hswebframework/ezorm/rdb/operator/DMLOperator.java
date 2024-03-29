package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.rdb.mapping.SyncRepository;
import org.hswebframework.ezorm.rdb.mapping.defaults.record.Record;
import org.hswebframework.ezorm.rdb.metadata.RDBTableMetadata;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;
import org.hswebframework.ezorm.rdb.operator.dml.upsert.UpsertOperator;

/**
 * DML操作接口,提供增删改查操作
 *
 * @author zhouhao
 * @since 4.0
 */
public interface DMLOperator {

    /**
     * 根据表结构来进行查询
     *
     * @param tableOrView TableOrViewMetadata
     * @return QueryOperator
     */
    QueryOperator query(TableOrViewMetadata tableOrView);

    DeleteOperator delete(RDBTableMetadata table);

    UpdateOperator update(RDBTableMetadata table);

    InsertOperator insert(RDBTableMetadata table);

    UpsertOperator upsert(RDBTableMetadata table);

    QueryOperator query(String tableOrView);

    UpdateOperator update(String table);

    InsertOperator insert(String table);

    DeleteOperator delete(String table);

    UpsertOperator upsert(String table);

    <K> SyncRepository<Record, K> createRepository(String tableName);

    <K> ReactiveRepository<Record, K> createReactiveRepository(String tableName);

}
