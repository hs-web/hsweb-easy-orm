package org.hsweb.ezorm.rdb;

import org.hsweb.ezorm.core.Database;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.meta.builder.TableBuilder;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;

import java.sql.SQLException;

/**
 * 数据库操作接口
 * Created by zhouhao on 16-6-4.
 */
public interface RDBDatabase extends Database {

    /**
     * 获取数据库定义对象
     *
     * @return 数据库定义对象
     */
    RDBDatabaseMetaData getMeta();

    /**
     * 获取一个表操作接口,如果数据库定义对象里未找到表结构定义,则尝试使用{@link TableMetaParser#parse(String)}进行解析
     *
     * @param name 表名
     * @param <T>  表数据泛型
     * @return 表操作接口
     */
    <T> RDBTable<T> getTable(String name);

    /**
     * 创建表,在数据库中创建表,如果表已存在,将不进行任何操作
     *
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 表操作接口
     * @throws SQLException 创建异常信息
     */
    <T> RDBTable<T> createTable(RDBTableMetaData tableMetaData) throws SQLException;

    /**
     * 重新载入结构定义,此操作不会对数据库表结构进行任何操作
     *
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 表操作接口
     */
    <T> RDBTable<T> reloadTable(RDBTableMetaData tableMetaData);

    /**
     * 变更表结构,此操作将修改表结构,如果存在删除的字段,且表中无数据,将删除字段
     *
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 修改后的表操作接口
     * @throws SQLException 修改异常
     */
    <T> RDBTable<T> alterTable(RDBTableMetaData tableMetaData) throws SQLException;

    /**
     * 删除表,此操作只会删除结构定义,不会删除物理数据库中的表
     *
     * @param name 表名
     * @return
     */
    boolean removeTable(String name);

    TableBuilder createOrAlter(String name);

}
