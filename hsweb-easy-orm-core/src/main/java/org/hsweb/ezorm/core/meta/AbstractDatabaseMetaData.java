package org.hsweb.ezorm.core.meta;

import org.hsweb.ezorm.core.ObjectWrapperFactory;
import org.hsweb.ezorm.core.ValidatorFactory;
import org.hsweb.ezorm.core.meta.storage.MapTableMetaDataStorage;
import org.hsweb.ezorm.core.meta.storage.TableMetaDataStorage;

public abstract class AbstractDatabaseMetaData implements DatabaseMetaData {
    protected ObjectWrapperFactory objectWrapperFactory;
    protected ValidatorFactory     validatorFactory;
    protected TableMetaDataStorage tableMetaDataStorage=new MapTableMetaDataStorage();

    @Override
    public <T extends TableMetaData> T getTableMetaData(String name) {
        return tableMetaDataStorage.getTableMetaData(name);
    }

    @Override
    public ObjectWrapperFactory getObjectWrapperFactory() {
        return objectWrapperFactory;
    }

    @Override
    public ValidatorFactory getValidatorFactory() {
        return validatorFactory;
    }

    public void setObjectWrapperFactory(ObjectWrapperFactory objectWrapperFactory) {
        this.objectWrapperFactory = objectWrapperFactory;
    }

    public void setValidatorFactory(ValidatorFactory validatorFactory) {
        this.validatorFactory = validatorFactory;
    }

    public void setTableMetaDataStorage(TableMetaDataStorage tableMetaDataStorage) {
        this.tableMetaDataStorage = tableMetaDataStorage;
    }
}
