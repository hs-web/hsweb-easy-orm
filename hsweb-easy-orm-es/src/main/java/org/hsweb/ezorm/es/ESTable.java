package org.hsweb.ezorm.es;

import org.hsweb.ezorm.core.Table;
import org.hsweb.ezorm.es.meta.ESTableMetaData;

/**
 * @author zhouhao
 */
public interface ESTable<T> extends Table<T> {
    @Override
    ESTableMetaData getMeta();

    @Override
    ESQuery<T> createQuery();

    @Override
    ESUpdate<T> createUpdate();

    @Override
    ESDelete createDelete();

    @Override
    ESInsert<T> createInsert();
}
