/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.rdb.executor;


import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.rdb.meta.expand.SimpleMapWrapper;
import org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 * SQL执行器,用于执行sql
 *
 * @author zhouhao
 * @since 1.0
 */
public interface SqlExecutor {

    SimpleMapWrapper mapWrapper = new SimpleMapWrapper();

    /**
     * 传入SQL对象和对象包装器执行查询,将查询结果通过对象包装器进行包装后返回
     *
     * @param sql     sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @param wrapper 执行结果对象包装器 参照{@link org.hsweb.ezorm.rdb.meta.expand.SimpleMapWrapper}
     * @param <T>     查询结果类型泛型
     * @return 查询结果
     * @throws SQLException 执行查询异常
     */
    <T> List<T> list(SQL sql, ObjectWrapper<T> wrapper) throws SQLException;

    /**
     * 传入SQL对象和对象包装器执行查询,将查询结果通过对象包装器进行包装后返回
     * 只返回单个结果,如果sql结果存在多个值,则返回首个值
     *
     * @param sql     sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @param wrapper 对象包装其
     * @param <T>     查询结果类型泛型
     * @return 查询结果
     * @throws SQLException 执行查询异常
     */
    <T> T single(SQL sql, ObjectWrapper<T> wrapper) throws SQLException;

    /**
     * 执行sql
     *
     * @param sql sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @throws SQLException 执行异常
     */
    void exec(SQL sql) throws SQLException;

    /**
     * 执行update
     *
     * @param sql sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @return 执行sql后影响的行数
     * @throws SQLException 执行异常
     */
    int update(SQL sql) throws SQLException;

    /**
     * 执行delete
     *
     * @param sql sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @return 执行sql后影响的行数
     * @throws SQLException 执行异常
     */
    int delete(SQL sql) throws SQLException;

    /**
     * 执行insert
     *
     * @param sql sql对象 参照{@link org.hsweb.ezorm.rdb.render.support.simple.SimpleSQL}
     * @return 执行sql后影响的行数
     * @throws SQLException 执行异常
     */
    int insert(SQL sql) throws SQLException;


    boolean tableExists(String tableName) throws SQLException;

    default List<Map<String, Object>> list(SQL sql) throws SQLException {
        return list(sql, mapWrapper);
    }

    default List<Map<String, Object>> list(String sql, Object params) throws SQLException {
        return list(new SimpleSQL(sql, params));
    }

    default List<Map<String, Object>> list(String sql) throws SQLException {
        return list(new SimpleSQL(sql));
    }

    default Map<String, Object> single(SQL sql) throws SQLException {
        return single(sql, mapWrapper);
    }

    default Map<String, Object> single(String sql, Object params) throws SQLException {
        return single(new SimpleSQL(sql, params));
    }

    default Map<String, Object> single(String sql) throws SQLException {
        return single(new SimpleSQL(sql));
    }

    default int insert(String sql, Object params) throws SQLException {
        return insert(new SimpleSQL(sql, params));
    }

    default int update(String sql, Object params) throws SQLException {
        return update(new SimpleSQL(sql, params));
    }

    default int update(String sql) throws SQLException {
        return update(new SimpleSQL(sql));
    }

    default int delete(String sql, Object params) throws SQLException {
        return delete(new SimpleSQL(sql, params));
    }

    default int delete(String sql) throws SQLException {
        return delete(new SimpleSQL(sql));
    }

    default void exec(String sql) throws SQLException {
        exec(new SimpleSQL(sql));
    }


}
