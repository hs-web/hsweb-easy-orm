package org.hsweb.ezorm.render.support.sqlserver;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.render.SqlRender;

/**
 * sqlServer 表结构修改sql渲染器,用于渲染sqlServer修改表的sql
 */
public class SqlServerMetaAlterRender implements SqlRender<Boolean> {

    @Override
    public SQL render(TableMetaData metaData, Boolean executeRemove) {
        // TODO: 16-9-29
        throw new UnsupportedOperationException();
    }
}
