package og.hsweb.ezorm.render.dialect;

import og.hsweb.ezorm.render.Dialect;

public class H2DatabaseMeta extends AbstractDatabaseMeta {
    private static final String DEFAULT_NAME = "h2";

    private String name;

    public H2DatabaseMeta() {
        name = DEFAULT_NAME;
        init();
    }

    public H2DatabaseMeta(String name) {
        this.name = name;
    }

    @Override
    public Dialect getDialect() {
        return Dialect.H2;
    }

    @Override
    public String getName() {
        return name;
    }
}
