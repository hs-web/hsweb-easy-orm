package org.hsweb.ezorm.rdb.simple;

import org.hsweb.ezorm.core.Delete;
import org.hsweb.ezorm.core.Insert;
import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.core.Update;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.RDBQuery;
import org.hsweb.ezorm.rdb.RDBTable;

/**
 * Created by zhouhao on 16-6-4.
 */
class SimpleTable<T> implements RDBTable<T> {
    private RDBTableMetaData metaData;

    private SqlExecutor sqlExecutor;

    private ObjectWrapper objectWrapper;

    private SimpleDatabase database;

    public SimpleTable(RDBTableMetaData metaData, SimpleDatabase database, SqlExecutor sqlExecutor, ObjectWrapper objectWrapper) {
        this.metaData = metaData;
        this.sqlExecutor = sqlExecutor;
        this.objectWrapper = objectWrapper;
        this.database = database;
    }

    @Override
    public RDBTableMetaData getMeta() {
        return metaData;
    }

    @Override
    public RDBQuery createQuery() {
        return new SimpleQuery<>(this, sqlExecutor, objectWrapper);
    }

    @Override
    public Update createUpdate() {
        return new SimpleUpdate<>(this, sqlExecutor);
    }

    @Override
    public Delete createDelete() {
        return new SimpleDelete(this, sqlExecutor);
    }

    @Override
    public Insert<T> createInsert() {
        return new SimpleInsert<>(this, sqlExecutor);
    }

    public SimpleDatabase getDatabase() {
        return database;
    }
}
