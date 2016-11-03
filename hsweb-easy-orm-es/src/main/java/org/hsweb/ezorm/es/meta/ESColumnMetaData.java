package org.hsweb.ezorm.es.meta;

import org.hsweb.ezorm.core.meta.AbstractColumnMetaData;

public class ESColumnMetaData extends AbstractColumnMetaData {

    @Override
    public ESTableMetaData getTableMetaData() {
        return super.getTableMetaData();
    }

    @Override
    public ESColumnMetaData clone() {
        ESColumnMetaData metaData = new ESColumnMetaData();
        metaData.setName(this.name);
        metaData.setAlias(this.alias);
        metaData.setComment(this.comment);
        metaData.setJavaType(this.javaType);
        metaData.setOptionConverter(this.optionConverter);
        metaData.setProperties(this.properties);
        metaData.setTableMetaData(this.tableMetaData);
        metaData.setValidator(this.validator);
        metaData.setValueConverter(this.valueConverter);
        return metaData;
    }
}
