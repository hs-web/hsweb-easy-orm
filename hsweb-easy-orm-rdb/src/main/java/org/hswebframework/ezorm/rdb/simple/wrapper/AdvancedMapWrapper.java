package org.hswebframework.ezorm.rdb.simple.wrapper;

import org.hswebframework.ezorm.core.DictionaryCodec;
import org.hswebframework.ezorm.core.ValueCodec;
import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;
import org.hswebframework.ezorm.rdb.meta.RDBTableMetaData;
import org.hswebframework.ezorm.rdb.meta.expand.SimpleMapWrapper;

import java.util.Map;

public class AdvancedMapWrapper extends SimpleMapWrapper {
    private RDBTableMetaData tableMetaData;


    public AdvancedMapWrapper(RDBTableMetaData tableMetaData) {
        this.tableMetaData = tableMetaData;
    }

    @Override
    public void wrapper(Map<String, Object> instance, int index, String attr, Object value) {
        RDBColumnMetaData metaData = tableMetaData.findColumn(attr);
        if (null != metaData) {
            ValueCodec valueCodec = metaData.getValueCodec();
            value = valueCodec.decode(value);
            super.wrapper(instance, index, attr, value);
            DictionaryCodec dictionaryCodec = metaData.getDictionaryCodec();
            if (dictionaryCodec != null) {
                Object value1 = dictionaryCodec.decode(value);
                String targetName = dictionaryCodec.getFieldName();
                if (attr.contains(".")) {
                    targetName = attr.split("[.]")[0] + "." + targetName;
                }
                putValue(instance, targetName, value1);
            }
        } else {
            super.wrapper(instance, index, attr, value);
        }
    }
}
