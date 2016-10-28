package org.hsweb.ezorm.rdb.render.support.sqlserver;

import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.render.SqlRender;

/**
 * sqlServer 表结构修改sql渲染器,用于渲染sqlServer修改表的sql
 */
public class SqlServerMetaAlterRender implements SqlRender<Boolean> {

    @Override
    public SQL render(RDBTableMetaData metaData, Boolean executeRemove) {
        // TODO: 16-9-29
        throw new UnsupportedOperationException();
    }
}
