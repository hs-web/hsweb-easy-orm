package org.hswebframework.ezorm.rdb.meta;

import org.hswebframework.ezorm.core.meta.AbstractDatabaseMetaData;
import org.hswebframework.ezorm.core.meta.DatabaseMetaData;
import org.hswebframework.ezorm.rdb.meta.parser.TableMetaParser;
import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.render.SqlRender_;

public abstract class RDBDatabaseMetaData extends AbstractDatabaseMetaData implements DatabaseMetaData {
    public abstract Dialect getDialect();

    public abstract void init();

}
