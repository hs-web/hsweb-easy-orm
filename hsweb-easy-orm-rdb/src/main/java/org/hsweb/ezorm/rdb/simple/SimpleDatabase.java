package org.hsweb.ezorm.rdb.simple;

import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.core.ObjectWrapperFactory;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBDatabaseMetaData;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.meta.builder.TableBuilder;
import org.hsweb.ezorm.rdb.meta.builder.simple.SimpleTableBuilder;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.RDBDatabase;
import org.hsweb.ezorm.rdb.RDBTable;
import org.hsweb.ezorm.rdb.simple.wrapper.AdvancedMapWrapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class SimpleDatabase implements RDBDatabase {
    private ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    private RDBDatabaseMetaData metaData;
    private SqlExecutor         sqlExecutor;
    private boolean autoParse = false;
    private Logger  logger    = LoggerFactory.getLogger(this.getClass());

    public SimpleDatabase(RDBDatabaseMetaData metaData, SqlExecutor sqlExecutor) {
        this.metaData = metaData;
        this.sqlExecutor = sqlExecutor;
    }

    protected Map<String, RDBTable> cache = new HashMap<>();

    @Override
    public RDBDatabaseMetaData getMeta() {
        return metaData;
    }

    @Override
    public <T> RDBTable<T> getTable(String name) {
        RDBTable table;
        RDBTableMetaData tableMetaData = metaData.getTableMetaData(name);
        if (tableMetaData == null) {
            if (metaData.getParser() != null && autoParse)
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
                ObjectWrapper wrapper = tableMetaData.getObjectWrapper();
                if (wrapper == null) {
                    ObjectWrapperFactory factory = metaData.getObjectWrapperFactory();
                    if (factory != null) {
                        wrapper = factory.createObjectWrapper(tableMetaData);
                    } else {
                        wrapper = new AdvancedMapWrapper(tableMetaData);
                    }
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
    public <T> RDBTable<T> createTable(RDBTableMetaData tableMetaData) throws SQLException {
        SqlRender render = metaData.getRenderer(SqlRender.TYPE.META_CREATE);
        SQL sql = render.render(tableMetaData, new Object());
        sqlExecutor.exec(sql);
        metaData.putTable(tableMetaData);
        return getTable(tableMetaData.getName());
    }

    @Override
    public <T> RDBTable<T> reloadTable(RDBTableMetaData tableMetaData) {
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
    public <T> RDBTable<T> alterTable(RDBTableMetaData tableMetaData) throws SQLException {
        SqlRender<Boolean> render = metaData.getRenderer(SqlRender.TYPE.META_ALTER);
        RDBTable old = getTable(tableMetaData.getName());
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
            metaData.removeTable(name);
            return true;
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public Map<String, Object> getTriggerContextRoot() {
        return new HashMap<>();
    }

    @Override
    public TableBuilder createOrAlter(String name) {
        RDBTableMetaData tableMetaData = new RDBTableMetaData();
        tableMetaData.setName(name);
        tableMetaData.setDatabaseMetaData(metaData);
        try {
            boolean tableExists;
            if (metaData.getParser() != null) {
                tableExists = metaData.getParser().tableExists(name);
            } else {
                tableExists = sqlExecutor.tableExists(name);
            }
            if (tableExists) {
                if (metaData.getParser() != null) {
                    RDBTableMetaData tmp = metaData.getParser().parse(name);
                    tmp.getColumns().forEach(tableMetaData::addColumn);
                } else {
                    logger.warn("table {} exists,but tableMetaParser is null");
                }
            }
        } catch (Exception e) {
        }
        return new SimpleTableBuilder(tableMetaData, this, sqlExecutor);
    }

    public void setAutoParse(boolean autoParse) {
        this.autoParse = autoParse;
    }

    public boolean isAutoParse() {
        return autoParse;
    }
}
