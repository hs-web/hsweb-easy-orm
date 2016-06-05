package og.hsweb.ezorm.render.dialect;

import og.hsweb.ezorm.render.Dialect;
import og.hsweb.ezorm.render.SqlRender;
import og.hsweb.ezorm.render.support.oracle.OracleMetaCreateRender;

public class OracleDatabaseMeta extends AbstractDatabaseMeta {
    private static final String DEFAULT_NAME = "oracle";

    private String name;

    @Override
    public void init() {
        super.init();
        renderMap.put(SqlRender.TYPE.META_CREATE, new OracleMetaCreateRender());
    }

    public OracleDatabaseMeta() {
        name = DEFAULT_NAME;
        init();
    }

    public OracleDatabaseMeta(String name) {
        this.name = name;
    }

    @Override
    public Dialect getDialect() {
        return Dialect.ORACLE;
    }

    @Override
    public String getName() {
        return name;
    }
}
