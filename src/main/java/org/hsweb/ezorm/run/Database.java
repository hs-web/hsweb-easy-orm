package org.hsweb.ezorm.run;

import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.parser.TableMetaParser;

import java.sql.SQLException;

/**
 * 数据库操作接口
 * Created by zhouhao on 16-6-4.
 */
public interface Database {

    /**
     * 获取数据库定义对象
     *
     * @return 数据库定义对象
     */
    DatabaseMetaData getMeta();

    /**
     * 获取一个表操作接口,如果数据库定义对象里未找到表结构定义,则尝试使用{@link TableMetaParser#parse(String)}进行解析
     *
     * @param name 表名
     * @param <T>  表数据泛型
     * @return 表操作接口
     */
    <T> Table<T> getTable(String name);

    /**
     * 创建表,在数据库中创建表,如果表已存在,将不进行任何操作
     *
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 表操作接口
     * @throws SQLException 创建异常信息
     */
    <T> Table<T> createTable(TableMetaData tableMetaData) throws SQLException;

    /**
     * 重新载入结构定义,此操作不会对数据库表结构进行任何操作
     *
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 表操作接口
     */
    <T> Table<T> reloadTable(TableMetaData tableMetaData);

    /**
     * 变更表结构,此操作将修改表结构,如果存在删除的字段,且表中无数据,将删除字段
     * @param tableMetaData 表结构定义
     * @param <T>           表数据泛型
     * @return 修改后的表操作接口
     * @throws SQLException 修改异常
     */
    <T> Table<T> alterTable(TableMetaData tableMetaData) throws SQLException;

    /**
     * 删除表,此操作只会删除结构定义,不会删除物理数据库中的表
     *
     * @param name 表名
     * @return
     */
    boolean removeTable(String name);

}
