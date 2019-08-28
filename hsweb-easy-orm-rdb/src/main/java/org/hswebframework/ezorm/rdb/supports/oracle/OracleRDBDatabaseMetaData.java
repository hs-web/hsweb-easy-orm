package org.hswebframework.ezorm.rdb.supports.oracle;

import org.hswebframework.ezorm.rdb.dialect.Dialect;
import org.hswebframework.ezorm.rdb.render.SqlRender_;
import org.hswebframework.ezorm.rdb.render.dialect.AbstractRDBDatabaseMetaData;

public class OracleRDBDatabaseMetaData extends AbstractRDBDatabaseMetaData {
    private static final String DEFAULT_NAME = "oracle";

    private String name;

    public OracleRDBDatabaseMetaData() {
        super(Dialect.ORACLE);
        name = DEFAULT_NAME;
        init();
    }

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender_.TYPE.META_CREATE, new OracleMetaCreateRender());
        renderMap.put(SqlRender_.TYPE.META_ALTER, new OracleMetaAlterRender(this));
    }

    @Override
    public String getName() {
        return name;
    }
}
