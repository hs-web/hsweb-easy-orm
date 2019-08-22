package org.hswebframework.ezorm.rdb.supports.h2;

import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.supports.commons.AbstractMetaAlterRender;

public class H2MetaAlterRender extends AbstractMetaAlterRender {
    public H2MetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
        super(databaseMetaData);
    }
}
