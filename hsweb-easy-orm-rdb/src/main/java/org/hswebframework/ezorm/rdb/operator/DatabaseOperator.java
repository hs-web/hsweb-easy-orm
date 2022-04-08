package org.hswebframework.ezorm.rdb.operator;

import org.hswebframework.ezorm.rdb.metadata.RDBDatabaseMetadata;

/**
 * 数据库操作底层接口,用于对数据库进行DDL,DML以及执行SQL等操作
 *
 * @see DMLOperator
 * @see DDLOperator
 * @see SQLOperator
 * @since 4.0
 */
public interface DatabaseOperator {

    /**
     * 获取数据库元数据
     *
     * @return 数据库元数据
     * @see RDBDatabaseMetadata
     */
    RDBDatabaseMetadata getMetadata();

    /**
     * 获取DML操作接口
     *
     * @return DMLOperator
     */
    DMLOperator dml();

    /**
     * 获取DDL操作接口
     *
     * @return DDLOperator
     */
    DDLOperator ddl();

    /**
     * 获取SQL操作接口
     *
     * @return SQLOperator
     */
    SQLOperator sql();

}
