package og.hsweb.ezorm.render;

import og.hsweb.ezorm.executor.SQL;
import og.hsweb.ezorm.meta.TableMetaData;

public interface SqlRender<R> {
    SQL render(TableMetaData metaData, R param);

    enum TYPE{
        INSERT,DELETE,UPDATE,SELECT,SELECT_TOTAL,META_ALTER,META_CREATE,META_DROP
    }
}
