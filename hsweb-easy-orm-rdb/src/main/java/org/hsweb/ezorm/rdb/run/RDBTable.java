package org.hsweb.ezorm.rdb.run;

import org.hsweb.ezorm.core.Table;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;

public interface RDBTable<T> extends Table<T> {
    RDBTableMetaData getMeta();

    RDBQuery<T> createQuery();

}
