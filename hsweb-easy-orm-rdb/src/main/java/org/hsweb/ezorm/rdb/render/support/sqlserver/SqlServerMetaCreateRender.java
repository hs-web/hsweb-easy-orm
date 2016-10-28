package org.hsweb.ezorm.rdb.render.support.sqlserver;

import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.render.SqlRender;

/**
 * sqlServer 表结构创建 sql渲染器,用于渲染sqlServer创建表的sql
 */
public class SqlServerMetaCreateRender implements SqlRender {

    @Override
    public SQL render(RDBTableMetaData metaData, Object param) {
        // TODO: 16-9-29
        throw new UnsupportedOperationException();
    }
}
