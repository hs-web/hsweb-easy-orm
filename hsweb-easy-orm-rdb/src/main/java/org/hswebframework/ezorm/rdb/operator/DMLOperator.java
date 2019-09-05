package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.operator.dml.QueryOperator;
import org.hswebframework.ezorm.rdb.operator.dml.delete.DeleteOperator;
import org.hswebframework.ezorm.rdb.operator.dml.insert.InsertOperator;
import org.hswebframework.ezorm.rdb.operator.dml.update.UpdateOperator;

public interface DMLOperator {

    QueryOperator query();

    UpdateOperator update(String table);

    InsertOperator insert(String table);

    DeleteOperator delete(String table);

}
