package org.hswebframework.ezorm.rdb.render.support.h2;

import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.render.support.simple.AbstractMetaAlterRender;

public class H2MetaAlterRender extends AbstractMetaAlterRender {
    public H2MetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
        super(databaseMetaData);
    }
}
