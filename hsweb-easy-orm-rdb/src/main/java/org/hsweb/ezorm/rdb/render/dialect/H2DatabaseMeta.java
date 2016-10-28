package org.hsweb.ezorm.rdb.render.dialect;

import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.render.support.h2.H2MetaAlterRender;
import org.hsweb.ezorm.rdb.render.support.oracle.OracleMetaCreateRender;

public class H2DatabaseMeta extends AbstractDatabaseMeta {
    private static final String DEFAULT_NAME = "h2";

    private String name;

    public H2DatabaseMeta() {
        super(new H2Dialect());
        name = DEFAULT_NAME;
        init();
    }

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender.TYPE.META_CREATE, new OracleMetaCreateRender());
        renderMap.put(SqlRender.TYPE.META_ALTER, new H2MetaAlterRender(this));
    }

    @Override
    public String getName() {
        return name;
    }
}
