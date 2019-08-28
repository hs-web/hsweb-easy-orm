package org.hswebframework.ezorm.rdb.render.dialect;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetaData;
import org.hswebframework.ezorm.core.meta.DatabaseMetaData;
import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hswebframework.ezorm.rdb.render.SqlRender_;
import org.hswebframework.ezorm.rdb.supports.commons.*;

import java.util.HashMap;
import java.util.Map;

public abstract class AbstractRDBDatabaseMetaData extends AbstractDatabaseMetaData implements DatabaseMetaData {
    protected Dialect dialect;

    public AbstractRDBDatabaseMetaData(Dialect dialect) {
        this.dialect = dialect;
    }

    public void init() {

    }

    public Dialect getDialect() {
        return dialect;
    }

}
