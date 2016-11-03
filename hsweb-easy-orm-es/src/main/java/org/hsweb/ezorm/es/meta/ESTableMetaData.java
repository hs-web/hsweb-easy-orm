package org.hsweb.ezorm.es.meta;

import org.hsweb.ezorm.core.meta.AbstractTableMetaData;
import org.hsweb.ezorm.core.meta.ColumnMetaData;
import org.hsweb.ezorm.core.meta.DatabaseMetaData;

public class ESTableMetaData extends AbstractTableMetaData<ESColumnMetaData> {

    private String idColumn = "id";
    private ESDatabaseMetaData databaseMetaData;

    @Override
    public ESColumnMetaData findColumn(String name) {
        return null;
    }

    public ESTableMetaData addColumn(ESColumnMetaData columnMetaData) {
        return super.addColumn(columnMetaData);
    }

    @Override
    public ESDatabaseMetaData getDatabaseMetaData() {
        return databaseMetaData;
    }

    public void setDatabaseMetaData(ESDatabaseMetaData databaseMetaData) {
        this.databaseMetaData = databaseMetaData;
    }

    public String getIdColumn() {
        return idColumn;
    }

    public void setIdColumn(String idColumn) {
        this.idColumn = idColumn;
    }
}
