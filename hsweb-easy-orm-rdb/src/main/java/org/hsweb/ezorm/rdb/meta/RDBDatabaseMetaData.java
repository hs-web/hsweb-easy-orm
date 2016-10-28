package org.hsweb.ezorm.rdb.meta;

import org.hsweb.ezorm.core.meta.DatabaseMetaData;
import org.hsweb.ezorm.core.ObjectWrapperFactory;
import org.hsweb.ezorm.core.ValidatorFactory;
import org.hsweb.ezorm.rdb.meta.parser.TableMetaParser;
import org.hsweb.ezorm.rdb.render.Dialect;
import org.hsweb.ezorm.rdb.render.SqlRender;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public abstract class RDBDatabaseMetaData implements DatabaseMetaData {
    private ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    private ValidatorFactory     validatorFactory;
    private ObjectWrapperFactory objectWrapperFactory;
    private Map<String, RDBTableMetaData> tables      = new HashMap<>();
    private Map<String, RDBTableMetaData> aliasTables = new HashMap<>();
    private TableMetaParser parser;
    public abstract Dialect getDialect();

    public abstract void init();

    public abstract SqlRender getRenderer(SqlRender.TYPE type);

    public RDBTableMetaData putTable(RDBTableMetaData table) {
        try {
            readWriteLock.writeLock().lock();
            table.setDatabaseMetaData(this);
            if (validatorFactory != null) {
                table.setValidator(validatorFactory.createValidator(table));
            }
            if (!table.getAlias().equals(table.getName())) {
                aliasTables.put(table.getAlias(), table);
            }
            return tables.put(table.getName(), table);
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public RDBTableMetaData remove(String name) {
        RDBTableMetaData metaData = getTable(name);
        if (metaData == null) return null;
        try {
            readWriteLock.writeLock().lock();
            metaData = tables.remove(metaData.getName());
            if (metaData == null)
                metaData = aliasTables.remove(metaData.getAlias());
            return metaData;
        } finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public RDBTableMetaData getTable(String name) {
        try {
            readWriteLock.readLock().lock();
            RDBTableMetaData metaData = tables.get(name);
            if (metaData == null) metaData = aliasTables.get(name);
            return metaData;
        } finally {
            readWriteLock.readLock().unlock();
        }
    }

    public void setValidatorFactory(ValidatorFactory validatorFactory) {
        this.validatorFactory = validatorFactory;
    }

    public ValidatorFactory getValidatorFactory() {
        return validatorFactory;
    }

    public abstract String getName();

    public ObjectWrapperFactory getObjectWrapperFactory() {
        return objectWrapperFactory;
    }

    public Map<String, RDBTableMetaData> getTables() {
        return tables;
    }

    public Map<String, RDBTableMetaData> getAliasTables() {
        return aliasTables;
    }

    public void setObjectWrapperFactory(ObjectWrapperFactory objectWrapperFactory) {
        this.objectWrapperFactory = objectWrapperFactory;
    }

    public void setParser(TableMetaParser parser) {
        this.parser = parser;
    }

    public TableMetaParser getParser() {
        return parser;
    }
}
