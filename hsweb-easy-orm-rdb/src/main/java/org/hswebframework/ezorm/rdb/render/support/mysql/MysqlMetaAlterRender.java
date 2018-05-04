package org.hswebframework.ezorm.rdb.render.support.mysql;

import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.render.support.simple.AbstractMetaAlterRender;


public class MysqlMetaAlterRender extends AbstractMetaAlterRender {

    public MysqlMetaAlterRender(RDBDatabaseMetaData databaseMetaData) {
        super(databaseMetaData);
    }

}
