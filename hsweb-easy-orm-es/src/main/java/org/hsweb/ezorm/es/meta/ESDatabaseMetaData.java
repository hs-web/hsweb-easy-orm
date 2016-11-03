package org.hsweb.ezorm.es.meta;

import org.hsweb.ezorm.core.meta.AbstractDatabaseMetaData;

public class ESDatabaseMetaData extends AbstractDatabaseMetaData {
    String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public ESTableMetaData getTableMetaData(String name) {
        return super.getTableMetaData(name);
    }

    public ESTableMetaData putTable(ESTableMetaData tableMetaData) {
        tableMetaData.setDatabaseMetaData(this);
        tableMetaDataStorage.putTableMetaData(tableMetaData);
        return tableMetaData;
    }
}
