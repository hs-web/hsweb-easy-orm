package org.hsweb.ezorm.meta;

import org.hsweb.ezorm.meta.expand.ObjectWrapperFactory;
import org.hsweb.ezorm.meta.expand.ValidatorFactory;
import org.hsweb.ezorm.meta.parser.TableMetaParser;
import org.hsweb.ezorm.render.Dialect;
import org.hsweb.ezorm.render.SqlRender;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public abstract class DatabaseMetaData {
    private ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    private ValidatorFactory validatorFactory;
    private ObjectWrapperFactory objectWrapperFactory;
    private Map<String, TableMetaData> tables = new HashMap<>();
    private Map<String, TableMetaData> aliasTables = new HashMap<>();
    private TableMetaParser parser;

    public abstract Dialect getDialect();

    public abstract void init();

    public abstract SqlRender getRenderer(SqlRender.TYPE type);

    public TableMetaData putTable(TableMetaData table) {
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

    public TableMetaData remove(String name) {
        TableMetaData metaData = getTable(name);
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

    public TableMetaData getTable(String name) {
        try {
            readWriteLock.readLock().lock();
            TableMetaData metaData = tables.get(name);
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

    public Map<String, TableMetaData> getTables() {
        return tables;
    }

    public Map<String, TableMetaData> getAliasTables() {
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
