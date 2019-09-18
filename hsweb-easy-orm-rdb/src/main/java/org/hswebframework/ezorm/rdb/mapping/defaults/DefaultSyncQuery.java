package org.hswebframework.ezorm.rdb.mapping.defaults;

import org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrapper;
import org.hswebframework.ezorm.rdb.mapping.SyncQuery;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;
import org.hswebframework.ezorm.rdb.operator.DMLOperator;

import java.util.List;
import java.util.Optional;

import static org.hswebframework.ezorm.rdb.executor.wrapper.ResultWrappers.*;
import static org.hswebframework.ezorm.rdb.operator.dml.query.Selects.count1;

public class DefaultSyncQuery<T>  extends DefaultQuery<T, SyncQuery<T>> implements SyncQuery<T> {

    public DefaultSyncQuery(TableOrViewMetadata tableMetadata, Class<T> entityType, DMLOperator operator, ResultWrapper<T, ?> wrapper) {
         super(tableMetadata, entityType, operator, wrapper);
    }

    @Override
    public List<T> fetch() {
        return operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .when(param.isPaging(), query -> query.paging(param.getPageIndex(), param.getPageSize()))
                .fetch(list(wrapper))
                .sync();
    }

    @Override
    public Optional<T> fetchOne() {
        return operator
                .query(tableName)
                .select(getSelectColumn())
                .where(param.getTerms())
                .orderBy(getSortOrder())
                .paging(0, 1)
                .fetch(optional(single(wrapper)))
                .sync();
    }

    @Override
    public int count() {
        return operator
                .query(tableName)
                .select(count1().as("total"))
                .where(param.getTerms())
                .fetch(optional(single(column("total", Number.class::cast))))
                .sync()
                .map(Number::intValue)
                .orElse(0);
    }


}
