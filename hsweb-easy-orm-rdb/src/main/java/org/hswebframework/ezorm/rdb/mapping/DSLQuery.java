package org.hswebframework.ezorm.rdb.mapping;

import org.hswebframework.ezorm.core.Conditional;
import org.hswebframework.ezorm.core.MethodReferenceColumn;
import org.hswebframework.ezorm.core.StaticMethodReferenceColumn;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.operator.dml.SortOrderSupplier;
import org.hswebframework.ezorm.rdb.operator.dml.query.SortOrder;

import java.util.Arrays;

@SuppressWarnings("all")
public interface DSLQuery<ME extends DSLQuery> extends Conditional<ME> {

    ME select(String... columns);

    ME selectExcludes(String... columns);

    default <T> ME select(StaticMethodReferenceColumn<T>... column) {
        return select(Arrays.stream(column).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    default <T> ME select(MethodReferenceColumn<T>... column) {
        return select(Arrays.stream(column).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    default <T> ME selectExcludes(StaticMethodReferenceColumn<T>... column) {
        return selectExcludes(Arrays.stream(column).map(StaticMethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    default <T> ME selectExcludes(MethodReferenceColumn<T>... column) {
        return selectExcludes(Arrays.stream(column).map(MethodReferenceColumn::getColumn).toArray(String[]::new));
    }

    ME paging(int pageIndex, int pageSize);

    ME orderBy(SortOrder... orders);

    ME orderBy(SortOrderSupplier... orders);

    ME setParam(QueryParam param);

}
