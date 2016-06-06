package org.hsweb.ezorm.run;

import org.hsweb.ezorm.meta.TableMetaData;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Table<T> {
    TableMetaData getMeta();

    Query<T> createQuery();

    Update<T> createUpdate();

    Insert<T> createInsert();

    Delete createDelete();
}
