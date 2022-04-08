package org.hswebframework.ezorm.rdb.executor;

import org.hswebframework.ezorm.core.FeatureId;
import org.hswebframework.ezorm.core.meta.Feature;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers;
import org.hswebframework.ezorm.rdb.metadata.RDBFeatureType;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

/**
 * 同步sql执行器,用于执行sql,并同步获取执行结果
 *
 * @author zhouhao
 * @since 4.0
 */
public interface SyncSqlExecutor extends Feature {

    String ID_VALUE = "syncSqlExecutor";

    FeatureId<SyncSqlExecutor> ID = FeatureId.of(ID_VALUE);

    @Override
    default String getId() {
        return ID_VALUE;
    }

    @Override
    default String getName() {
        return "同步SQL执行器";
    }

    @Override
    default RDBFeatureType getType() {
        return RDBFeatureType.sqlExecutor;
    }

    /**
     * 执行更新,可用于执行update insert delete 语句
     *
     * @param request sql请求
     * @return 影响的记录条数
     * @see SqlRequests
     * @see BatchSqlRequest
     */
    int update(SqlRequest request);

    /**
     * 执行SQL,通常用于执行DDL操作等语句
     *
     * @param request sql请求
     * @see SqlRequests
     * @see BatchSqlRequest
     */
    void execute(SqlRequest request);

    /**
     * 执行查询语句
     * <pre>
     *   sqlExecutor.select(of("select * from user where name = ?",name),single(map()));
     * </pre>
     *
     * @param request SQL请求
     * @param wrapper 查询结果包装器
     * @param <T>     行类型
     * @param <R>     结果类型
     * @return 查询结果
     * @see SqlRequests
     * @see org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers
     */
    <T, R> R select(SqlRequest request, ResultWrapper<T, R> wrapper);

    default int update(String sql, Object... args) {
        return update(SqlRequests.of(sql, args));
    }

    default <T, R> R select(String sql, ResultWrapper<T, R> wrapper) {
        return select(SqlRequests.of(sql), wrapper);
    }

    default List<Map<String, Object>> select(String sql, Object... args) {
        return select(SqlRequests.of(sql, args), ResultWrappers.mapList());
    }

}
