package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.param.QueryParam;

import java.util.function.Supplier;

/**
 * 动态DSL删除接口,用于通过DSL方式构造动态删除条件
 *
 * @param <ME> 实现此接口的类型
 * @author zhouhao
 * @see QueryParam
 * @since 4.0.0
 */
public interface DSLDelete<ME extends DSLDelete<?>> extends Conditional<ME> {

    /**
     * 将删除条件转为查询条件,通常用于在删除前,查询可能被删除的数据
     *
     * @return 查询条件
     */
    QueryParam toQueryParam();

    /**
     * 将更新条件转为查询条件,通常用于根据更新来查询可能被更新的数据
     *
     * @return 动态查询条件
     */
    <T extends QueryParam> T toQueryParam(Supplier<T> template);


}
