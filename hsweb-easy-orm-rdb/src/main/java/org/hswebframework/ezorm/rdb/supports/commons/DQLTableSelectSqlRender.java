package org.hswebframework.ezorm.rdb.supports.commons;

import org.hswebframework.ezorm.core.meta.ObjectType;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.rdb.executor.SqlRequest;
import org.hswebframework.ezorm.rdb.meta.RDBObjectType;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.render.RenderType;
import org.hswebframework.ezorm.rdb.render.SqlRender;

public class DQLTableSelectSqlRender<T extends RDBTableMetaData> implements SqlRender<T, Object> {
    @Override
    public ObjectType getObjectType() {
        return RDBObjectType.table;
    }

    @Override
    public RenderType getRenderType() {
        return RenderType.dql_select;
    }

    @Override
    public SqlRequest render(T metadata, Object parameter) {
        QueryParam param= transToQueryParam(parameter);



        return null;
    }

    public QueryParam transToQueryParam(Object parameter) {
        if (parameter instanceof QueryParam) {
            return ((QueryParam) parameter);
        }

        throw new UnsupportedOperationException("parameter type " + parameter.getClass() + " not support yet.");
    }
}
