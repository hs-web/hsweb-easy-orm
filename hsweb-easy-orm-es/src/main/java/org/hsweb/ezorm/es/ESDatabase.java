package org.hsweb.ezorm.es;

import org.hsweb.ezorm.core.Database;
import org.hsweb.ezorm.es.meta.ESDatabaseMetaData;

/**
 * @author zhouhao
 */
public interface ESDatabase extends Database {
    @Override
    ESDatabaseMetaData getMeta();

    <T> ESTable<T> getTable(String name);

}
