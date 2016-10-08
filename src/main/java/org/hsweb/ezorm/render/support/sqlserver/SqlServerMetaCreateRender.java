package org.hsweb.ezorm.render.support.sqlserver;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.render.SqlRender;

/**
 * sqlServer 表结构创建 sql渲染器,用于渲染sqlServer创建表的sql
 */
public class SqlServerMetaCreateRender implements SqlRender {

    @Override
    public SQL render(TableMetaData metaData, Object param) {
        // TODO: 16-9-29
        throw new UnsupportedOperationException();
    }
}
