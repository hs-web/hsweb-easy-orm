package org.hswebframework.ezorm.rdb;

import org.hswebframework.ezorm.core.Table;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;

public interface RDBTable<T> extends Table<T> {
    RDBTableMetaData getMeta();

    RDBQuery<T> createQuery();

}
