package org.hswebframework.ezorm.rdb.dialect;

import org.hswebframework.ezorm.rdb.meta.RDBColumnMetaData;

 public interface DataTypeMapper {
    String getDataType(RDBColumnMetaData columnMetaData);
}
