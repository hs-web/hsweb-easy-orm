package org.hsweb.ezorm.run.simple;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.meta.expand.ObjectWrapper;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.DatabaseMetaData;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.expand.ObjectWrapperFactory;
import org.hsweb.ezorm.render.SqlRender;
import org.hsweb.ezorm.render.support.simple.SimpleSQL;
import org.hsweb.ezorm.run.Database;
import org.hsweb.ezorm.run.Table;
import org.hsweb.ezorm.run.simple.wrapper.AdvancedMapWrapper;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleDatabase implements Database {
    private ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    private DatabaseMetaData metaData;
    private SqlExecutor sqlExecutor;

    public SimpleDatabase(DatabaseMetaData metaData, SqlExecutor sqlExecutor) {
        this.metaData = metaData;
        this.sqlExecutor = sqlExecutor;
    }

    protected Map<String, Table> cache = new HashMap<>();

    @Override
    public DatabaseMetaData getMeta() {
        return metaData;
    }

    @Override
    public <T> Table<T> getTable(String name) {
        Table table;
        TableMetaData tableMetaData = metaData.getTable(name);
        if (tableMetaData == null) {
            if (metaData.getParser() != null)
                tableMetaData = metaData.getParser().parse(name);
            if (tableMetaData != null) {
                metaData.putTable(tableMetaData);
            } else
                throw new NullPointerException("表不存在!");
        }
        try {
            readWriteLock.readLock().lock();
            table = cache.get(name);
            if (null != table) return table;
        } finally {
            readWriteLock.readLock().unlock();
        }
        if (table == null) {
            try {
                readWriteLock.writeLock().lock();
                ObjectWrapper wrapper;
                ObjectWrapperFactory factory = metaData.getObjectWrapperFactory();
                if (factory != null) {
                    wrapper = factory.createObjectWrapper(tableMetaData);
                } else {
                    wrapper = new AdvancedMapWrapper(tableMetaData);
                }
                table = new SimpleTable(tableMetaData, this, sqlExecutor, wrapper);
                cache.put(name, table);
            } finally {
                readWriteLock.writeLock().unlock();
            }
        }
        return table;
    }

    @Override
    public <T> Table<T> createTable(TableMetaData tableMetaData) throws SQLException {
        SimpleQuery.TotalWrapper wrapper = new SimpleQuery.TotalWrapper();
        try {
            sqlExecutor.single(new SimpleSQL( "select count(0) from " + tableMetaData.getName() + " where 1=2", new Object()), wrapper);
        } catch (Exception e) {
            SqlRender render = metaData.getRenderer(SqlRender.TYPE.META_CREATE);
            SQL sql = render.render(tableMetaData, new Object());
            sqlExecutor.exec(sql);
            metaData.putTable(tableMetaData);
        }
        return getTable(tableMetaData.getName());
    }

    @Override
    public <T> Table<T> reloadTable(TableMetaData tableMetaData) {
        try {
            readWriteLock.writeLock().lock();
            cache.remove(tableMetaData.getName());
            cache.remove(tableMetaData.getAlias());
            metaData.putTable(tableMetaData);
        } finally {
            readWriteLock.writeLock().unlock();
        }
        return getTable(tableMetaData.getAlias());
    }

    @Override
    public <T> Table<T> alterTable(TableMetaData tableMetaData) throws SQLException {
        SqlRender<Boolean> render = metaData.getRenderer(SqlRender.TYPE.META_ALTER);
        Table old = getTable(tableMetaData.getName());
        if (old == null) throw new NullPointerException("旧表不存在!");
        int total = old.createQuery().total();
        SQL sql = render.render(tableMetaData, total == 0);
        try {
            readWriteLock.writeLock().lock();
            sqlExecutor.exec(sql);
        } finally {
            readWriteLock.writeLock().unlock();
        }
        reloadTable(tableMetaData);
        return getTable(tableMetaData.getName());
    }

    @Override
    public boolean removeTable(String name) {
        try {
            readWriteLock.writeLock().lock();
            return metaData.remove(name) != null;
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public Map<String, Object> getTriggerContextRoot() {
        return new HashMap<>();
    }
}
