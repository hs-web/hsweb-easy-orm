package org.hsweb.ezorm.rdb;

import org.hsweb.ezorm.core.Table;
import org.hsweb.ezorm.core.dsl.Query;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;

public interface RDBTable<T> extends Table<T> {
    RDBTableMetaData getMeta();

    RDBQuery<T> createQuery();

}
