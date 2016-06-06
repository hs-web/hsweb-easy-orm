package org.hsweb.ezorm.run.simple;

import org.hsweb.ezorm.meta.expand.ObjectWrapper;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.run.*;

/**
 * Created by zhouhao on 16-6-4.
 */
class SimpleTable<T> implements Table<T> {
    private TableMetaData metaData;

    private SqlExecutor sqlExecutor;

    private ObjectWrapper objectWrapper;

    private SimpleDatabase database;

    public SimpleTable(TableMetaData metaData, SimpleDatabase database, SqlExecutor sqlExecutor, ObjectWrapper objectWrapper) {
        this.metaData = metaData;
        this.sqlExecutor = sqlExecutor;
        this.objectWrapper = objectWrapper;
        this.database = database;
    }

    @Override
    public TableMetaData getMeta() {
        return metaData;
    }

    @Override
    public Query createQuery() {
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
