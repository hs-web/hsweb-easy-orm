package org.hswebframework.ezorm.rdb.meta.dialect;

import org.hswebframework.ezorm.rdb.meta.RDBColumnMetadata;

 public interface DataTypeMapper {
    String getDataType(RDBColumnMetadata columnMetaData);
}
