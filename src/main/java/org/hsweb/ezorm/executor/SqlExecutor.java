package org.hsweb.ezorm.executor;


import org.hsweb.ezorm.meta.expand.ObjectWrapper;

import java.sql.SQLException;
import java.util.List;

/**
 * SQL执行器
 * Created by 浩 on 2015-11-09 0009.
 */
public interface SqlExecutor {

    /**
     * 传入SQL对象和对象包装器执行查询,将查询结果通过对象包装器进行包装后返回
     *
     * @param sql     sql对象
     * @param wrapper 执行结果对象包装器
     * @param <T>     查询结果类型泛型
     * @return 查询结果
     * @throws Exception 执行查询异常
     */
    <T> List<T> list(SQL sql, ObjectWrapper<T> wrapper) throws SQLException;

    /**
     * 传入SQL对象和对象包装器执行查询,将查询结果通过对象包装器进行包装后返回
     * 只返回单个结果,如果sql结果存在多个值,则返回首个值
     *
     * @param sql     sql对象
     * @param wrapper 对象包装其
     * @param <T>     查询结果类型泛型
     * @return 查询结果
     * @throws Exception 执行查询异常
     */
    <T> T single(SQL sql, ObjectWrapper<T> wrapper) throws SQLException;

    /**
     * 执行sql
     *
     * @param sql sql对象
     * @throws Exception 执行异常
     */
    void exec(SQL sql) throws SQLException;

    /**
     * 执行update
     *
     * @param sql sql对象
     * @return 执行sql后影响的行数
     * @throws Exception 执行异常
     */
    int update(SQL sql) throws SQLException;

    /**
     * 执行delete
     *
     * @param sql sql对象
     * @return 执行sql后影响的行数
     * @throws Exception 执行异常
     */
    int delete(SQL sql) throws SQLException;

    /**
     * 执行insert
     *
     * @param sql sql对象
     * @return 执行sql后影响的行数
     * @throws Exception 执行异常
     */
    int insert(SQL sql) throws SQLException;

}
